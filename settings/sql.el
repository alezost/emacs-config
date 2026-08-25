;;; sql.el --- Settings for `sql' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'sql)
(require 'al-places)
(require 'al-general)
(require 'al-key)
(require 'al-sql)

(defconst al/sql-keys
  '(("C-v"   . sql-send-region)
    ("C-M-v" . sql-send-paragraph)
    ("M-s-v" . sql-send-buffer)
    ("C-c C-z" . al/sql-switch-to-repl))
  "Alist of auxiliary keys for `sql-mode'.")
(al/bind-keys-from-vars 'sql-mode-map 'al/sql-keys)

;; I just can't stand the default key bindings.
(al/clean-keymap sql-interactive-mode-map)

(setq
 sql-product 'postgres
 sql-database "darts"
 sql-user user-login-name
 sql-connection-alist
 `((darts (sql-product 'postgres)
          (sql-server "")
          (sql-database "darts")
          (sql-user ,user-login-name))
   (ptmp  (sql-product 'postgres)
          (sql-server "")
          (sql-database "tmp")
          (sql-user ,user-login-name))
   (mtmp  (sql-product 'mariadb)
          (sql-server "")
          (sql-database "tmp")
          (sql-user ,user-login-name)))

 al/sql-history-dir (al/emacs-data-dir-file "sql"))

(al/call-at-hook sql-mode-hook
  al/sql-set-comment-start-skip)
(al/call-at-hook sql-interactive-mode-hook
  al/sql-save-history
  al/sql-highlight-product
  al/sql-completion-setup)

(advice-add 'sql-highlight-product
  :override 'al/sql-highlight-product)

;; Fix bug with mariadb prompt:
;; <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17426>.
(sql-set-product-feature 'mysql :prompt-regexp
                         "^\\(?:mysql\\|mariadb\\).*> ")

;;; sql.el ends here
