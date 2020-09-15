; vim: ft=lisp et
(in-package :asdf)
(defsystem "rpjson.test"
  :version
  "0.0.0"
  :depends-on
  (:jingoh "rpjson")
  :components
  ((:file "rpjson"))
  :perform
  (test-op (o c) (declare (special args))
   (apply #'symbol-call :jingoh :examine :rpjson args)))