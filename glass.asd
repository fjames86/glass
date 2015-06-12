;;;; Copyright (c) Frank James 2015 <frank.a.james@gmail.com>
;;;; This code is licensed under the MIT license.

(asdf:defsystem :glass
  :name "frpc"
  :author "Frank James <frank.a.james@gmail.com>"
  :description "General Lisp Authentication and Security System API."
  :license "MIT"
  :version "1.0.2"
  :components
  ((:file "glass")
   (:file "errors" :depends-on ("glass"))))
