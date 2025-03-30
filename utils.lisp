(in-package :piccolo)

(defvar *piccolo-version* "v0.0.7")

(defparameter *history-pointer* 0 
  "History pointer is used to keep track of where we are when going back and forward")

(defun parse-gopher-url (url)
  "Parse a Gopher URL and return (host selector port) with defaults."
  (let* ((gopher-uri (quri:uri url))
         (scheme (quri:uri-scheme gopher-uri))
         (host (quri:uri-host gopher-uri))
         (port (quri:uri-port gopher-uri))
         (selector (quri:uri-path gopher-uri)))
    (values
     (string= scheme "gopher")
     (if (> (length host) 0)
         host 
         nil)
     (if port
         (parse-integer port :junk-allowed t)
         70)
     (if selector
         selector
         "/"))))

(defun add-to-history (host port selector)
  (piccolo::save-history host port selector)
  (setf *history-pointer* (piccolo::get-max-history-id)))

(defun show-help ()
  (format t "~%Piccolo - CLI Gopher Client ~A~%~%" *piccolo-version*)
  (format t "~A"
          "Command Line Usage:
  piccolo [gopher://host[:port]/selector]

Input Options:
  [0-9]*  Navigate to a menu item
  b       Back
  f       Forward
  o       Home
  d       Favorite
  q       Quit
  h       Show this help

Examples:
  piccolo gopher://gopher.floodgap.com/
  piccolo
"))
