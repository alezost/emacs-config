;;; utl-mysql.el --- Additional functionality for mysql package

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 18 Jan 2015

;;; Code:

(require 'mysql)

;; XXX The code of `mysql' and `sql-completion' is horrible.  It would
;; be better to fork these packages and fix (rewrite) everything.
;;
;; Sources:
;;
;; - https://code.google.com/p/ywb-codes/source/browse/trunk/emacs/site-lisp/contrib/mysql.el
;; - https://code.google.com/p/ywb-codes/source/browse/trunk/emacs/site-lisp/contrib/sql-completion.el

(defun utl-mysql-shell-query (sql &optional db)
  "Same as `mysql-shell-query' but works.
In `mysql-shell-query' SQL code does not quoted properly, so the
shell command may fail."
  (let ((cmd (mapconcat
              'identity
              (append (append (list mysql-program)
                              ;; -s option inhibit header in output
                              (remove "-s" mysql-options))
                      (list
                       "-u" mysql-user
                       db
                       (and (string< "" mysql-password)
                            (concat "-p" mysql-password))
                       ;; This line (↓) is the only difference.
                       "-e" (shell-quote-argument sql)))
              " ")))
    (mysql-output-table (shell-command-to-string cmd))))

(provide 'utl-mysql)

;;; utl-mysql.el ends here
