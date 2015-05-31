
(asdf:defsystem :retrospectiff-test
  :name "retrospectiff-test"
  :description "test library for retrospectiff"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class cl-source-file
  :depends-on (retrospectiff)
  :components
  ((:module :test
            :serial t
            :components ((:file "package")
                         (:file "retrospectiff-test")))))
