
(asdf:defsystem :retrospectiff
  :name "retrospectiff"
  :description "A library for reading and writing TIFF images"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :version "0.1.1"
  :licence "BSD"
  :serial t
  :depends-on (com.gigamonkeys.binary-data
               flexi-streams
               ieee-floats
               cl-jpeg
               deflate)
  :components
  ((:cl-source-file "package")
   (:cl-source-file "util")
   (:cl-source-file "bit-array")
   (:cl-source-file "lzw")
   (:cl-source-file "jpeg")
   (:cl-source-file "deflate")
   (:cl-source-file "packbits")
   (:cl-source-file "constants")
   (:cl-source-file "retrospectiff"))
  :in-order-to ((test-op (test-op :retrospectiff/test))))

(asdf:defsystem :retrospectiff/test
  :name "retrospectiff/tests"
  :description "test library for retrospectiff"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class cl-source-file
  :depends-on (retrospectiff fiveam)
  :components
  ((:module :test
            :serial t
            :components ((:file "package")
                         (:file "retrospectiff-test"))))
  :perform (test-op (o c)
                    (uiop:symbol-call :fiveam '#:run! :retrospectiff)))
