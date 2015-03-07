(defpackage #:apktools.axml
  (:use #:cl)
  (:export #:read-axml-file
           #:read-axml)
  (:import-from #:nibbles #:read-ub32/le #:read-ub16/le #:ub16ref/le)
  (:import-from #:babel #:octets-to-string)
  (:import-from #:ieee-floats #:decode-float32))

(in-package #:apktools.axml)

(defconstant chunk-null                #x00000000)
(defconstant chunk-xml-file            #x00080003)
(defconstant chunk-resource-ids        #x00080180)
(defconstant chunk-xml-start-namespace #x00100100)
(defconstant chunk-xml-end-namespace   #x00100101)
(defconstant chunk-xml-start-tag       #x00100102)
(defconstant chunk-xml-end-tag         #x00100103)
(defconstant chunk-xml-text            #x00100104)
(defconstant chunk-string-pool         #x001C0001)

(defclass context ()
  ((handler :initarg :handler :reader context-handler)
   (string-pool :initform nil :accessor context-string-pool)
   (resource-ids :initform nil :accessor context-resource-ids)))

(defun read-axml-file (filename handler)
  (with-open-file (stream filename
                          :direction :input
                          :element-type '(unsigned-byte 8))
    (read-axml stream handler)))

(defun read-axml (stream handler)
  (unless (= (read-ub32/le stream) chunk-xml-file)
    (error "Not a valid AXML file."))
  (read-ub32/le stream)
  (let ((context (make-instance 'context :handler handler)))
    (sax:start-document handler)
    (handler-case (loop (read-chunk stream context))
      (end-of-file ()))
    (sax:end-document handler)))

(defgeneric read-chunk-payload (stream type size context))

(defun read-chunk (stream context)
  (let ((start-offset (file-position stream))
        (type (loop for x = (read-ub32/le stream)
                    while (= x chunk-null)
                    finally (return x)))
        (size (read-ub32/le stream)))
    (read-chunk-payload stream type size context)
    (file-position stream (+ start-offset size))))

(defmethod read-chunk-payload (stream type size context)
  (declare (ignore stream context))
  (warn "Unrecognized chunk type: 0x~8,'0X  Size: ~D." type size))

;;; String pool reader

(defconstant utf8-flag #x100)

(defmethod read-chunk-payload (stream (type (eql chunk-string-pool)) size context)
  (let* ((num-strings (read-ub32/le stream))
         (num-styles (read-ub32/le stream))
         (flags (read-ub32/le stream))
         (offset-strings (read-ub32/le stream))
         (offset-styles (read-ub32/le stream))
         (string-offsets (loop repeat num-strings
                               collect (read-ub32/le stream)))
         (style-offsets (loop repeat num-styles
                              collect (read-ub32/le stream)))
         (buffer-size (check-divisible "Buffer size"
                                       (- (if (zerop offset-styles)
                                              size
                                              offset-styles)
                                          offset-strings)
                                       4))
         (buffer (make-array buffer-size
                             :element-type '(unsigned-byte 8))))
    (declare (ignore style-offsets))
    (read-sequence buffer stream)
    (setf (context-string-pool context)
          (map 'vector
               (lambda (offset)
                 (extract-string buffer offset (logtest flags utf8-flag)))
               string-offsets))))

(defun check-divisible (name x y)
  (unless (zerop (mod x y))
    (error "~A (~D) did not divide by ~D." name x y))
  x)

(defun extract-string (buffer offset is-utf8)
  (flet ((extract (length-reader length-mul encoding)
           (multiple-value-bind (len k)
               (funcall length-reader buffer offset)
             (octets-to-string buffer :start (+ offset k)
                                      :end (+ offset k (* len length-mul))
                                      :encoding encoding))))
    (if is-utf8
        (extract #'read-varint 1 :utf-8)
        (extract #'read-short 2 :utf-16le))))

(defun read-short (buffer offset)
  (values (ub16ref/le buffer offset) 2))

(defun read-varint (buffer offset)
  (let* ((val (aref buffer offset))
         (more (logtest val #x80)))
    (if more
        (values (logior (ash (logand val #x7F) 8) (aref buffer (1+ offset))) 2)
        (values val 1))))

(defun context-string (id context)
  (if (= id (1- (expt 2 32)))
      ""
      (aref (context-string-pool context) id)))

;;; Resource IDs reader

(defmethod read-chunk-payload (stream (type (eql chunk-resource-ids)) size context)
  (setf (context-resource-ids context)
        (loop repeat (- (floor size 4) 2)
              collect (read-ub32/le stream))))

;;; XML Start Namespace reader

(defmethod read-chunk-payload (stream (type (eql chunk-xml-start-namespace)) size context)
  (let ((line-number (read-ub32/le stream))
        (reserved (read-ub32/le stream))
        (prefix (read-ub32/le stream))
        (uri (read-ub32/le stream)))
    (declare (ignore line-number reserved))
    (sax:start-prefix-mapping (context-handler context)
                              (context-string prefix context)
                              (context-string uri context))))

;;; XML End Namespace reader

(defmethod read-chunk-payload (stream (type (eql chunk-xml-end-namespace)) size context)
  (declare (ignore size))
  (let ((line-number (read-ub32/le stream))
        (reserved (read-ub32/le stream))
        (prefix (read-ub32/le stream))
        (uri (read-ub32/le stream)))
    (declare (ignore line-number reserved uri))
    (sax:end-prefix-mapping (context-handler context)
                            (context-string prefix context))))

;;; XML Start Tag reader

(defmethod read-chunk-payload (stream (type (eql chunk-xml-start-tag)) size context)
  (let* ((line-number (read-ub32/le stream))
         (reserved (read-ub32/le stream))
         (ns-uri (read-ub32/le stream))
         (name (read-ub32/le stream))
         (flags (read-ub32/le stream))
         (num-attrs (read-ub16/le stream))
         (id-attr (1- (read-ub16/le stream)))
         (class-attr (1- (read-ub16/le stream)))
         (style-attr (1- (read-ub16/le stream)))
         (attrs (loop repeat num-attrs
                      collect (read-attribute stream context))))
    (declare (ignore line-number reserved flags id-attr class-attr style-attr))
    (sax:start-element (context-handler context)
                       (context-string ns-uri context)
                       (context-string name context)
                       (context-string name context)
                       attrs)))

(defgeneric format-attribute-value (string type data context))

(defun read-attribute (stream context)
  (let ((ns-uri (read-ub32/le stream))
        (name (read-ub32/le stream))
        (value-string (read-ub32/le stream))
        (value-type (ash (read-ub32/le stream) -24))
        (value-data (read-ub32/le stream)))
    (sax:make-attribute :namespace-uri (context-string ns-uri context)
                        :local-name (context-string name context)
                        :qname (context-string name context)
                        :value (format-attribute-value value-string value-type value-data context)
                        :specified-p t)))

(defmacro define-attribute-value-format (type symbolic-name form)
  (declare (ignore symbolic-name))
  `(defmethod format-attribute-value (string (type (eql ,type)) data context)
     (declare (ignorable string data context))
     ,form))

(defun complex-to-float (x)
  (* (logand x #xFFFFFF00)
     (aref #(0.00390625 3.051758e-005 1.192093e-007 4.656613e-010)
           (logand (ash x -4) 3))))

(defun complex-unit (x kind)
  (aref (ecase kind
          (:dimension #("px" "dip" "sp" "pt" "in" "mm"))
          (:fraction #("%" "%p")))
        (logand x #x0F)))

(define-attribute-value-format 0 :null "")
(define-attribute-value-format 1 :reference (format nil "@~A~8,'0X" (get-package data) data))
(define-attribute-value-format 2 :attribute (format nil "?~A~8,'0X" (get-package data) data))
(define-attribute-value-format 3 :string (context-string string context))
(define-attribute-value-format 4 :float (format nil "~F" (decode-float32 data)))
(define-attribute-value-format 5 :dimension (format nil "~F~A" (complex-to-float data) (complex-unit data :dimension)))
(define-attribute-value-format 6 :fraction (format nil "~F~A" (* 100.0 (complex-to-float data)) (complex-unit data :fraction)))
(define-attribute-value-format 16 :int-dec (format nil "~D" data))
(define-attribute-value-format 17 :int-hex (format nil "~8,'0X" data))
(define-attribute-value-format 18 :boolean (if (zerop data) "false" "true"))
(define-attribute-value-format 28 :argb8 (format nil "#~8,'0X" data))
(define-attribute-value-format 29 :rgb8 (format nil "#~8,'0X" data))
(define-attribute-value-format 30 :argb4 (format nil "#~8,'0X" data))
(define-attribute-value-format 31 :rgb4 (format nil "#~8,'0X" data))

(defun get-package (x)
  (if (= 1 (ash x -24)) "android:" ""))

;;; XML End Tag reader

(defmethod read-chunk-payload (stream (type (eql chunk-xml-end-tag)) size context)
  (let ((line-number (read-ub32/le stream))
        (reserved (read-ub32/le stream))
        (ns-uri (read-ub32/le stream))
        (name (read-ub32/le stream)))
    (declare (ignore line-number reserved ns-uri))
    (sax:end-element (context-handler context)
                     (context-string ns-uri context)
                     (context-string name context)
                     (context-string name context))))

;;; XML Text reader

(defmethod read-chunk-payload (stream (type (eql chunk-xml-text)) size context)
  (let ((line-number (read-ub32/le stream))
        (reserved (read-ub32/le stream))
        (name (read-ub32/le stream)))
    (declare (ignore line-number reserved))
    (sax:characters (context-handler context)
                    (context-string name context))))
