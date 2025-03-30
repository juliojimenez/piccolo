(load "package.lisp")
(load "db.lisp")
(load "net.lisp")
(load "menu.lisp")
(load "nav.lisp")
(load "utils.lisp")
(load "main.lisp")

(p::init-db)

(format t "~%All modules loaded.~%Database initialized.~%")
