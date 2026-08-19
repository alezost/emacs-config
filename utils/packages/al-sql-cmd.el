;;; al-sql-cmd.el --- Additional commands for `sql' package  -*- lexical-binding: t -*-

;; Copyright © 2016–2026 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains "entry point" commands for `sql' package to avoid
;; recursive loading.  See `al-eshell-cmd' commentary for details.

;;; Code:

(eval-when-compile
  (require 'let-macros))
(require 'sql)
(require 'al-buffer)

;;;###autoload
(defun al/sql-switch-to-repl ()
  "Switch to SQLi buffer."
  (interactive)
  (unless (and sql-buffer
               (buffer-live-p (get-buffer sql-buffer)))
    (sql-set-sqli-buffer))
  (pop-to-buffer sql-buffer))

;;;###autoload
(defun al/sql-switch-or-connect (conn)
  "Switch to SQLi buffer with connection CONN.
Create it if it does not exist.
Interactively, use the first connection from `sql-connection-alist'.
With prefix, prompt for connection."
  (interactive
   (list (if current-prefix-arg
             (sql-read-connection "Connection: ")
           (caar sql-connection-alist))))
  (if-let ((buffer (sql-find-sqli-buffer)))
      (al/display-buffer buffer)
    (sql-connect conn)))

(provide 'al-sql-cmd)

;;; al-sql-cmd.el ends here
