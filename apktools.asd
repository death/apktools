;;;; +----------------------------------------------------------------+
;;;; | APK tools                                                      |
;;;; +----------------------------------------------------------------+

;;; System definition

;;; -*- Mode: LISP; Syntax: COMMON-LISP; Package: CL-USER; Base: 10 -*-

(asdf:defsystem #:apktools
  :description "Tools for dealing with Android Application Packages (APKs)"
  :author "death <github.com/death>"
  :license "MIT"
  :depends-on (#:babel #:nibbles #:ieee-floats #:cxml)
  :components
  ((:file "axml")))
