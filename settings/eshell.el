;;; eshell.el --- Settings for `eshell' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'esh-mode)
;; Set `eshell-directory-name' early because other modules use it to set
;; other variables (e.g. `eshell-aliases-file').
(setq eshell-directory-name (al/emacs-data-dir-file "eshell"))

(require 'em-prompt)
(require 'sh-script)
(require 'al-places)
(require 'al-key)
(require 'al-eshell)

(al/bind-keys
  :map eshell-mode-map
  ("RET"   'al/eshell-send-input-maybe)
  ("M-↑"   'eshell-previous-input)
  ("M-↓"   'eshell-next-input)
  ("M-S-↑" 'eshell-previous-prompt)
  ("M-S-↓" 'eshell-next-prompt)
  ("C-k"   'al/eshell-kill-whole-line)
  ("C-c r" 'al/eshell-refresh-aliases))

(al/bind-keys
  :map eshell-hist-mode-map
  "<up>" "<down>"
  ("M-r" 'al/eshell-previous-matching-input-from-input)
  ("M-s" 'al/eshell-next-matching-input-from-input))

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
