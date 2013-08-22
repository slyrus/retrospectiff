
(asdf:defsystem :retrospectiff-test
  :name "retrospectiff-test"
  :author "Cyrus Harmon <ch-lisp@bobobeach.com>"
  :licence "BSD"
  :default-component-class cl-source-file
  :depends-on (retrospectiff hu.dwim.stefil)
  :components
  ((:module :test
            :serial t
            :components ((:file "package")
                         (:file "retrospectiff-test")))))
