(in-package :cl-user)

(defpackage :piccolo
  (:use :common-lisp :usocket)
  (:nicknames :p)
  (:export 
   :run-with-args
   :display-gopher-menu))
