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
