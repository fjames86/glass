;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :glass
  :name "frpc"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "General Lisp API Security System."
  :license "MIT"
  :components
  ((:file "glass")
   (:file "errors" :depends-on ("glass"))))
