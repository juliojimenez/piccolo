(in-package :piccolo)

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

(defun display-gopher-menu (add-history host selector)
  "Fetch and display a Gopher menu in a readable format."
  (let* ((data (fetch-gopher add-history host selector))
         (menu-items (parse-gopher-menu data)))
    (format t "~%--- Gopher Menu from ~A ---~%~%" host)
    (loop for item in menu-items
          for index from 1
          for type = (getf item :type)
          for name = (getf item :name)
          for icon = (cond
                       ((string= type "1") "🗂️")  ;; Directory
                       ((string= type "0") "📄")  ;; File
                       ((string= type "7") "🔍")  ;; Search Server
                       (t type))
          do (if (string= type "i")
                 (format t "~A~%" name)
                 (format t "~2D. ~A ~A~%" index icon name)))
    menu-items))