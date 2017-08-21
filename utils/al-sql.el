;;; al-sql.el --- Additional functionality for sql stuff

;; Copyright © 2013–2017 Alex Kost

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

;;; Code:

(require 'sql)

;;;###autoload
(defun al/sql-switch-to-repl ()
  "Switch to SQLi buffer."
  (interactive)
  (unless (and sql-buffer
               (buffer-live-p (get-buffer sql-buffer)))
    (sql-set-sqli-buffer))
  (pop-to-buffer sql-buffer))


;;; SQL passwords from .authinfo

(require 'auth-source)

;;;###autoload
(defun al/sql-password-from-auth-source (host &optional user)
  "Return sql password from authinfo file by HOST and USER.
Return nil if credentials not found."
  (let ((auth (car (auth-source-search :host host :user user))))
    (when auth
      (let* ((secret (plist-get auth :secret))
             (password (if (functionp secret)
                           (funcall secret)
                         secret)))
        (or password "")))))

(declare-function sql-mysql-completion-init "sql-completion")

(defun al/sql-completion-setup ()
  "Setup `sql-completion' for the current sql interaction buffer."
  (and (require 'sql-completion nil t)
       (eq major-mode 'sql-interactive-mode)
       (eq sql-product 'mysql)
       (sql-mysql-completion-init)))


;;; Log of sql commands

;; Idea from <http://www.emacswiki.org/emacs/SqlMode>.  Add to .emacs:
;; (add-hook 'sql-interactive-mode-hook 'al/sql-save-history)

(defcustom al/sql-history-dir
  (expand-file-name "sql" user-emacs-directory)
  "Directory for history of sql commands."
  :type 'string
  :group 'sql)

;;;###autoload
(defun al/sql-save-history ()
  "Save a history of commands separately for each sql-product.
Use `al/sql-history-dir'."
  (if sql-product
      (setq-local sql-input-ring-file-name
                  (expand-file-name (concat (symbol-name sql-product)
                                            "-history.sql")
                                    al/sql-history-dir))
    (error "SQL history will not be saved because sql-product is nil")))


;;; Mode line

(require 'al-mode-line)

(defun al/sql-highlight-product ()
  "Add sql product name to `al/mode-info' instead of `mode-name'.
This function is intended to be used as a substitution for
`sql-highlight-product'."
  (when (derived-mode-p 'sql-mode)
    (set-syntax-table (sql-product-syntax-table))
    (sql-product-font-lock nil t))
  (and (or (derived-mode-p 'sql-mode)
           (derived-mode-p 'sql-interactive-mode))
       (setq al/mode-info
             (or (sql-get-product-feature sql-product :name)
                 (symbol-name sql-product)))))

(provide 'al-sql)

;;; al-sql.el ends here
