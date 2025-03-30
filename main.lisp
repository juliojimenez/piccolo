(in-package :piccolo)

(defun run ()
  "Start the piccolo CLI Gopher client."
  (format t "Welcome to piccolo - CLI Gopher Client ~A~%" *piccolo-version*)
  (multiple-value-bind (scheme host port selector) (parse-gopher-url (piccolo::get-setting "home"))
    (if (and scheme port)
        (navigate-gopher host selector))))
     

(defun run-with-args ()
    "Entry point for Piccolo with command-line argument handling."
    (let ((args (uiop:command-line-arguments)))
      (if (and args (not (string= (first args) "")))
          (multiple-value-bind (scheme host port selector) (parse-gopher-url (first args))
            (if scheme
                (if host
                    (progn
                      (format t "Connecting to ~A on port ~A with selector ~A~%" host port selector)
                      (navigate-gopher host selector))
                    (format t "Malformed host."))
                (format t "Only gopher:// URI scheme is supported.")))
          (piccolo::run))))
