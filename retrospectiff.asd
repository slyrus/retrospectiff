
(asdf:defsystem :retrospectiff
    :name "retrospectiff"
    :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
    :version #.(with-open-file
                   (vers (merge-pathnames "version.lisp-expr" *load-truename*))
                 (read vers))
    :licence "BSD"
    :components
    ((:static-file "version" :pathname #p"version.lisp-expr")
     (:static-file "COPYRIGHT")
     (:static-file "README")
     (:cl-source-file "package")
     (:cl-source-file "util" :depends-on (package))
     (:cl-source-file "bit-array" :depends-on (package))
     (:cl-source-file "lzw" :depends-on (package bit-array))
     (:cl-source-file "retrospectiff" :depends-on (package lzw util))))
