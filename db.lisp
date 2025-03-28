(in-package :piccolo)

(defparameter *db-path*
  (merge-pathnames "piccolo.db"
                   (merge-pathnames ".piccolo/"
                                    (uiop:getenv "HOME")))
  "Path to the SQLite database in ~/.piccolo/piccolo.db")

(defun ensure-db-dir-exists ()
  "Ensure that the ~/.piccolo directory exists."
  (let ((dir (pathname-directory *db-path*)))
    (unless (probe-file (make-pathname :directory dir))
      (uiop::ensure-directories-exist *db-path*))))

(defun init-db ()
  "Initialize the SQLite database with history and favorites tables."
  (ensure-db-dir-exists)
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-non-query db
                              "CREATE TABLE IF NOT EXISTS history (
                                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                                 host TEXT NOT NULL,
                                 port INTEGER NOT NULL,
                                 selector TEXT NOT NULL,
                                 visited_at TEXT NOT NULL
                               );")
    (sqlite:execute-non-query db
                              "CREATE TABLE IF NOT EXISTS favorites (
                                 id INTEGER PRIMARY KEY AUTOINCREMENT,
                                 name TEXT NOT NULL,
                                 host TEXT NOT NULL,
                                 port INTEGER NOT NULL,
                                 selector TEXT NOT NULL
                               );")))

(defun save-history (url)
  "Insert a visited URL into the history table."
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-non-query db
                              "INSERT INTO history (url, visited_at) VALUES (?, datetime('now'));"
                              url)))

(defun add-favorite (name url)
  "Insert a favorite into the favorites table."
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-non-query db
                              "INSERT INTO favorites (name, url) VALUES (?, ?);"
                              name url)))

(defun list-history ()
  "List the most recent history entries."
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-to-list db
                            "SELECT visited_at, url FROM history ORDER BY visited_at DESC LIMIT 20;")))

(defun list-favorites ()
  "List all saved favorites."
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-to-list db
                            "SELECT name, url FROM favorites ORDER BY name ASC;")))
