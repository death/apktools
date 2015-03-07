# APK Tools

Tools for dealing with Android Application Packages (APKs).

## Android binary XML (AXML) reader.

```lisp
CL-USER> (apktools.axml:read-axml-file "AndroidManifest.xml"
                                       (cxml-xmls:make-xmls-builder))
(("manifest" . "")
 ((("versionCode" . "http://schemas.android.com/apk/res/android") "1")
  (("versionName" . "http://schemas.android.com/apk/res/android") "1.0")
  (("package" . "") "org.adeht.doomsdayquiz")
  (("platformBuildVersionCode" . "") "19")
  (("platformBuildVersionName" . "") "4.4.2-1035858"))
 (("uses-sdk" . "")
  ((("minSdkVersion" . "http://schemas.android.com/apk/res/android") "11")
   (("targetSdkVersion" . "http://schemas.android.com/apk/res/android") "19")))
 (("application" . "")
  ((("theme" . "http://schemas.android.com/apk/res/android") "@7F070001")
   (("label" . "http://schemas.android.com/apk/res/android") "@7F060001")
   (("icon" . "http://schemas.android.com/apk/res/android") "@7F020000")
   (("debuggable" . "http://schemas.android.com/apk/res/android") "true")
   (("allowBackup" . "http://schemas.android.com/apk/res/android") "true"))
  (("activity" . "")
   ((("label" . "http://schemas.android.com/apk/res/android") "@7F060001")
    (("name" . "http://schemas.android.com/apk/res/android")
     "org.adeht.doomsdayquiz.MainActivity"))
   (("intent-filter" . "") NIL
    (("action" . "")
     ((("name" . "http://schemas.android.com/apk/res/android")
       "android.intent.action.MAIN")))
    (("category" . "")
     ((("name" . "http://schemas.android.com/apk/res/android")
       "android.intent.category.LAUNCHER")))))))
```

# Links

* [Androguard](https://github.com/androguard/androguard)

# License

MIT
