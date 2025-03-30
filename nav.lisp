(in-package :piccolo)

(defun navigate-gopher (host selector)
  "CLI Gopher browser loop."
  (loop
    (let ((menu (display-gopher-menu host selector)))
      (format t "~%Enter a number, (b)ack, or (h)elp: ")
      (force-output)
      (let ((input (read-line)))
        (cond
          ((string= input "q") (return))
          ((string= input "b") (setf selector "/"))  ;; Go back to root
          ((and (parse-integer input :junk-allowed t)
                (<= (parse-integer input) (length menu)))
           (let* ((choice (nth (1- (parse-integer input)) menu))
                  (type (getf choice :type))
                  (new-host (getf choice :host))
                  (new-selector (getf choice :selector)))
             (cond
               ;; If it's a Gopher menu, navigate to it
               ((string= type "1")
                (setf host new-host selector new-selector))
               
               ;; If it's a plain file (`0`), print its contents instead of parsing
               ((string= type "0")
                (format t "~%--- File Contents ---~%")
                (format t "~A~%" (fetch-gopher new-host new-selector))
                (format t "~%--- End of File ---~%")
                (format t "~%Press Enter to return to menu...~%")
                (read-line))
               
               (t (format t "Unsupported type: ~A~%" type)))))
          ((string= input "h")
           (show-help)
           (format t "~%Press Enter to return to menu...~%")
           (read-line))
          (t (format t "Invalid choice, try again.~%")))))))