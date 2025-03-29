(in-package :piccolo)

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
