(in-package :cl-user)

(defpackage :piccolo
  (:use :common-lisp :usocket)
  (:nicknames :p)
  (:export :run))

(in-package :piccolo)

;;; Test connection to a Gopher server
; (let ((socket (usocket:socket-connect "gopher.floodgap.com" 70)))
;   (if socket
;       (format t "Connection successful!~%")
;       (format t "Failed to connect.~%")))

;;; Fetch data from a Gopher server
(defun fetch-gopher (host selector &optional (port 70))
  "Fetch data from a Gopher server properly, handling stream reads correctly."
  (format t "Connecting to ~A:~A...~%" host port)
  (usocket:with-client-socket (socket stream host port)
    (format stream "~A~C~C" selector #\Return #\Newline) 
    (finish-output stream)
    ; (format t "Request sent! Checking if stream is open...~%")
    ; (if (listen stream)
    ;     (format t "Stream is open, ready to read.~%")
    ;     (format t "Stream is already closed!~%"))
    (format t "Reading response...~%")
    ;; Read until EOF
    (let ((response (make-array 0 :element-type 'character :adjustable t :fill-pointer 0)))
      (loop for char = (read-char stream nil :eof)
            while (not (eq char :eof))
            do (vector-push-extend char response))
      ;; Print full response (for debugging)
      ;; (format t "Full Response (~A bytes):~%~A~%" (length response) response)
      (coerce response 'string))))  ;; Convert array to string

(defun parse-gopher-menu (data)
  "Parse a Gopher menu response into a structured list."
  (loop for line in (cl-ppcre:split #\Newline data)
        ; do (format t "Processing line: ~A~%" line)  ;; Debugging output
        when (and (> (length line) 0) (not (string= line "."))) ;; Ignore `.` end marker
        collect (let* ((parts (split-sequence:split-sequence #\Tab line))
                       (type (subseq (first parts) 0 1))
                       (name (subseq (first parts) 1))
                       (selector (if (>= (length parts) 2) (second parts) ""))
                       (host (if (>= (length parts) 3) (third parts) ""))
                       (port (if (and (>= (length parts) 4) 
                                      (not (string= (fourth parts) "")))
                                 (parse-integer (fourth parts) :junk-allowed t)
                                 70)))  ;; Default port to 70 if missing
                  ;; Handle informational (`i`) lines
                  (if (string= type "i")
                      (list :type type :name name :selector nil :host nil :port nil)
                      (list :type type :name name :selector selector :host host :port port)))))


(defun display-gopher-menu (host selector)
  "Fetch and display a Gopher menu in a readable format."
  (let* ((data (fetch-gopher host selector))
         (menu-items (parse-gopher-menu data)))
    (format t "~%--- Gopher Menu from ~A ---~%~%" host)
    (loop for item in menu-items
          for index from 1
          for type = (getf item :type)
          for name = (getf item :name)
          for icon = (cond
                       ((string= type "1") "🗂️")  ;; Directory
                       ((string= type "0") "📄")  ;; File
                       (t type))
          do (if (string= type "i")
                 (format t "~A~%" name)
                 (format t "~2D. ~A ~A~%" index icon name)))
    menu-items))

(defun navigate-gopher (host selector)
  "CLI Gopher browser loop."
  (loop
    (let ((menu (display-gopher-menu host selector)))
      (format t "~%Enter choice (number), 'b' for back, or 'q' to quit: ")
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
                (format t "~%--- End of File ---~%"))
               
               (t (format t "Unsupported type: ~A~%" type)))))
          (t (format t "Invalid choice, try again.~%")))))))

(defun run ()
  "Start the piccolo CLI Gopher client."
  (format t "Welcome to piccolo - CLI Gopher Client~%")
  (navigate-gopher "gopher.quux.org" "/"))
