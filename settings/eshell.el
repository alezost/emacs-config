;;; eshell.el --- Settings for `eshell' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'esh-mode)
;; Set `eshell-directory-name' early because other modules use it to set
;; other variables (e.g. `eshell-aliases-file').
(setq eshell-directory-name (al/emacs-data-dir-file "eshell"))

(require 'sh-script)
(require 'al-places)
(require 'al-key)
(require 'al-eshell)

(defconst al/eshell-keys
  '(("C-c r" . al/eshell-refresh-aliases)
    ("RET" . al/eshell-send-input-maybe)
    ("C-k" . al/eshell-kill-whole-line)
    ("M-." . eshell-previous-input)
    ("M-e" . eshell-next-input)
    ("M->" . eshell-previous-prompt)
    ("M-E" . eshell-next-prompt))
  "Alist of auxiliary keys for `eshell-mode-map'.")
(al/bind-keys-from-vars 'eshell-mode-map 'al/eshell-keys)

(defconst al/eshell-hist-keys
  '("<up>" "<down>"
    ("M-r" . al/eshell-previous-matching-input-from-input)
    ("M-s" . al/eshell-next-matching-input-from-input))
  "Alist of auxiliary keys for `eshell-hist-mode-map'.")
(al/bind-keys-from-vars 'eshell-hist-mode-map 'al/eshell-hist-keys)

(setq
 eshell-modules-list
 '(eshell-alias
   eshell-basic
   eshell-cmpl
   eshell-dirs
   eshell-glob
   eshell-hist
   eshell-ls
   eshell-pred
   eshell-prompt
   eshell-script
   eshell-term
   eshell-unix
   eshell-tramp)

 eshell-prompt-function #'al/eshell-prompt
 ;; `sh-mode-syntax-table' has proper syntax for comments.
 eshell-mode-syntax-table sh-mode-syntax-table

 eshell-highlight-prompt nil

 eshell-hist-ignoredups t
 eshell-history-size 9999)

(add-hook 'eshell-mode-hook #'al/eshell-set-local-variables)

;; eshell does horrible thing with aliases: "alias foo" not only
;; removes "foo" alias from the current eshell buffer (which is
;; already bad enough), it also immediately overwrites (!)
;; `eshell-aliases-file'.  How could anyone come up with this
;; brilliant idea?
(advice-add 'eshell-write-aliases-list :override #'ignore)

(advice-add 'eshell/info :override #'al/eshell/info)

;; This mode does nothing except for binding keys that I don't need.
(advice-add 'eshell-cmpl-mode :override #'ignore)

;;; eshell.el ends here
