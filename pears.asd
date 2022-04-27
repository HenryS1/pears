(defsystem "pears"
  :version "1.0.0"
  :author "Henry Steere"
  :license "MIT"
  :depends-on ("salmon")
  :components ((:module "src"
                :components 
                ((:file "pears"))))
  :description "A monadic parser for Common Lisp"
  :in-order-to ((test-op (test-op "pears/tests"))))

;; (defsystem "pears/tests"
;;   :depends-on ("rove"
;;                "salmon")
;;   :components ((:module "tests"
;;                 :components
;;                 ((:file "maybe-tests")
;;                  (:file "try-tests")
;;                  (:file "monad-tests")
;;                  (:file "mlist-tests")
;;                  (:file "mvector-tests")
;;                  (:file "either-tests"))))
;;   :perform (test-op (o c) (symbol-call :rove '#:run c)))
