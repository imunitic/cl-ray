(defsystem "cl-ray"
  :version "0.0.1"
  :author "Ivica Munitic"
  :license "MIT"
  :depends-on ()
  :components ((:module "src"
                :components
                ((:file "main"))))
  :description "raytracer challenge project."
  :in-order-to ((test-op (test-op "cl-ray/tests"))))

(defsystem "cl-ray/tests"
  :author "Ivica Munitic"
  :license "MIT"
  :depends-on ("cl-ray"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cl-ray"
  :perform (test-op (op c) (symbol-call :rove :run c)))
