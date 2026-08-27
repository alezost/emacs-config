;;; sly.el --- Settings for `sly' package  -*- lexical-binding: t -*-

(require 'sly)
(require 'al-key)
(require 'al-clisp)
(require 'al-sly)

(al/bind-keys
  :map sly-xref-mode-map
  ("↑" 'sly-xref-prev-line)
  ("↓" 'sly-xref-next-line)
  ("→" 'sly-xref-goto)
  ("d" 'sly-xref-show))

(al/bind-keys
  :map sly-db-mode-map
  ("↑"   'sly-db-up)
  ("↓"   'sly-db-down)
  ("S-↑" 'sly-db-details-up)
  ("S-↓" 'sly-db-details-down)
  ("M-↑" 'sly-db-beginning-of-backtrace)
  ("M-↓" 'sly-db-end-of-backtrace))

(al/bind-keys
  :map sly-db-frame-map
  ([tab] 'sly-db-toggle-details)
  ("d"   'sly-db-show-frame-source)
  ("v"   'sly-db-eval-in-frame))

(al/bind-keys
  :map sly-doc-map
  ("C-d" 'sly-documentation-lookup))

(al/bind-keys
  :map sly-mrepl-mode-map
  ("C-c C-d" 'al/sly-repl-disconnect-or-quit)
  ("M-r"   'comint-history-isearch-backward-regexp)
  ("M-↑"   'sly-mrepl-previous-input-or-button)
  ("M-↓"   'sly-mrepl-next-input-or-button)
  ("M-S-↑" 'sly-mrepl-previous-prompt)
  ("M-S-↓" 'sly-mrepl-next-prompt))

;; `sly-editing-mode' is a useless wrapper for `sly-mode' but some
;; contrib modules add commands to its hook.  So making only `sly-mode'
;; work instead of `sly-editing-mode' would require too much
;; configuration.  At least, clean its keymap.
(al/clean-keymap sly-editing-mode-map)

(al/bind-keys
  ;; Do not remove this map (used by ERC settings).
  :map al/sly-map
  :create t
  ("C-c"   sly-prefix-map)
  ("C-v"   'al/sly-eval-dwim)
  ("C-M-v" 'sly-eval-defun)
  ("M-s-v" 'sly-eval-buffer)
  ("C-S-v" 'sly-macroexpand-all)
  ("C-d"   'sly-describe-symbol)
  ("M-d"   'sly-edit-definition)
  ("C-M-d" sly-doc-map))

(al/bind-keys
  :map sly-mode-map
  :parent al/sly-map)

(setq
 sly-contribs
 '(sly-mrepl
   sly-autodoc
   sly-fancy-inspector
   sly-fancy-trace
   sly-scratch
   sly-package-fu
   sly-trace-dialog
   sly-stickers
   sly-indentation
   sly-tramp))

;; Clearly, contrib packages should be configured ONCE after loading
;; sly.  Instead, `sly--setup-contribs' is called on EVERY connection
;; (by `sly-setup-connection') to give you double benefit: no contrib
;; modules before the first REPL start and useless reevaluating of the
;; same code multiple times.
(sly--setup-contribs)
(advice-add 'sly--setup-contribs :override #'ignore)

;; Bind `sly-prefix-map' keys after loading contribs because `sly-mrepl'
;; binds "C-c C-z".
(al/bind-keys
  :map sly-prefix-map
  "C-d"
  ("C-z" (al/sly 'other-window))
  ("C-j" 'al/sly-switch-to-repl-and-enter))

(al/clean-keymap sly-autodoc-mode-map)

(setq
 inferior-lisp-program "sbcl"
 sly--mode-line-format '(:eval (al/sly-mode-line-format)))

(advice-add 'sly-make-action-button
  :around #'al/sly-change-action-button-label)

;; Fix some indentation broken by `sly-cl-indent'.
(al/clisp-setup-indentation)

;;; sly.el ends here
