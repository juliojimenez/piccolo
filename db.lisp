(in-package :piccolo)

(defparameter *db-path*
  (merge-pathnames "piccolo.db"
                   (merge-pathnames ".piccolo/"
                                    (namestring (user-homedir-pathname))))
  "Path to the SQLite database in ~/.piccolo/piccolo.db")

(defun ensure-db-dir-exists ()
  "Ensure that the ~/.piccolo directory exists."
  (uiop::ensure-directories-exist *db-path*))

(defun save-setting (key value)
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-non-query db
                              "INSERT OR REPLACE INTO settings (key, value) VALUES (?, ?);"
                              key value)))

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
                               );")
    (sqlite:execute-non-query db
                              "CREATE TABLE IF NOT EXISTS settings (
                                 key TEXT PRIMARY KEY,
                                 value TEXT NOT NULL
                               );"))
  (save-setting "home" "gopher://gopher.quux.org"))

(defun save-history (host port selector)
  "Insert a visited URL into the history table."
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-non-query db
                              "INSERT INTO history (host, port, selector, visited_at) VALUES (?, ?, ?, datetime('now'));"
                              host port selector)))

(defun add-favorite (name host port selector)
  "Insert a favorite into the favorites table."
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-non-query db
                              "INSERT INTO favorites (name, host, port, selector) VALUES (?, ?, ?, ?);"
                              name host port selector)))

(defun list-history ()
  "List the most recent history entries."
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-to-list db
                            "SELECT * FROM history ORDER BY visited_at DESC LIMIT 20;")))

(defun list-favorites ()
  "List all saved favorites."
  (sqlite:with-open-database (db *db-path*)
    (sqlite:execute-to-list db
                            "SELECT * FROM favorites ORDER BY name ASC;")))

(defun get-max-history-id ()
  "Return the largest INTEGER PRIMARY KEY from the given table."
  (sqlite:with-open-database (db *db-path*)
    (let ((result (sqlite:execute-to-list db
                                          "SELECT MAX(id) FROM history;")))
      (if (and result (first result))
          (first (first result))
          0))))  ; or NIL if you want to distinguish empty tables

(defun get-setting (key &optional (default ""))
  (sqlite:with-open-database (db *db-path*)
    (let ((result (sqlite:execute-to-list db
                                          "SELECT value FROM settings WHERE key = ? LIMIT 1;"
                                          key)))
      (if result
          (first (first result))  ; get the value
          default))))
