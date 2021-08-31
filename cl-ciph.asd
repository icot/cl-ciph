(defsystem "cl-ciph"
  :version "0.1.1"
  :author "icot"
  :license "GPLv3"
  :depends-on ("str" "uiop" "unix-opts")
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

;(asdf:defsystem :system-name
  ;...
  ;:build-operation "asdf:program-op"
  ;:build-pathname "my-exe-name-relative-to-the-source-directory"
  ;:entry-point "my-package:my-launch-function")

;(sb-ext:save-lisp-and-die "myprog" :toplevel #'main :executable t :compression 9)
