;;; al-mysql.el --- Additional functionality for mysql package

;; Copyright © 2015-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'mysql)

;; XXX The code of `mysql' and `sql-completion' is horrible.  It would
;; be better to fork these packages and fix (rewrite) everything.
;;
;; Sources:
;;
;; - https://code.google.com/p/ywb-codes/source/browse/trunk/emacs/site-lisp/contrib/mysql.el
;; - https://code.google.com/p/ywb-codes/source/browse/trunk/emacs/site-lisp/contrib/sql-completion.el

(defun al/mysql-shell-query (sql &optional db)
  "Same as `mysql-shell-query' but works.
In `mysql-shell-query' SQL code is not quoted properly, so the
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

(provide 'al-mysql)

;;; al-mysql.el ends here
