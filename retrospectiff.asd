
(asdf:defsystem :retrospectiff
    :name "retrospectiff"
    :description "A library for reading and writing TIFF images"
    :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
    :version "0.1.0"
    :licence "BSD"
    :serial t
    :depends-on (com.gigamonkeys.binary-data
                 ieee-floats)
    :components
    ((:static-file "COPYRIGHT")
     (:static-file "README")
     (:cl-source-file "package")
     (:cl-source-file "util")
     (:cl-source-file "bit-array")
     (:cl-source-file "lzw")
     (:cl-source-file "packbits")
     (:cl-source-file "constants")
     (:cl-source-file "retrospectiff")))
