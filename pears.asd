(defsystem "pears"
  :version "1.0.0"
  :author "Henry Steere"
  :license "MIT"
  :components ((:module "src"
                :components 
                ((:file "pears"))))
  :description "A combinator parser for Common Lisp"
  :in-order-to ((test-op (test-op "pears/tests"))))

(defsystem "pears/examples"
  :components ((:module "examples"
                :components 
                ((:file "json-parser"))))
  :description "An example json parser using pears")

(defsystem "pears/tests"
  :depends-on ("rove")
  :components ((:module "tests"
                :components
                ((:file "parser-tests"))))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))

