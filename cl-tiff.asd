

(asdf:defsystem :cl-tiff
    :name "cl-tiff"
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
     (:cl-source-file "cl-tiff" :depends-on (package))))
