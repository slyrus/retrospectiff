
(asdf:defsystem :retrospectiff
    :name "retrospectiff"
    :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
    :version "0.0.5"
    :licence "BSD"
    :components
    ((:static-file "COPYRIGHT")
     (:static-file "README")
     (:cl-source-file "package")
     (:cl-source-file "util" :depends-on (package))
     (:cl-source-file "bit-array" :depends-on (package))
     (:cl-source-file "lzw" :depends-on (package bit-array))
     (:cl-source-file "packbits" :depends-on (package bit-array))
     (:cl-source-file "retrospectiff" :depends-on (package
                                                   util
                                                   lzw
                                                   packbits))))
