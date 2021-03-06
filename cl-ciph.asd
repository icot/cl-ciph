(defsystem "cl-ciph"
  :version "0.1.1"
  :author "icot"
  :license "GPLv3"
  :depends-on ("unix-opts")
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cl-ciph/tests"))))

(defsystem "cl-ciph/tests"
  :author "icot"
  :license "GPLv3"
  :depends-on ("cl-ciph"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-ciph"
  :perform (test-op (op c) (symbol-call :rove :run c)))
