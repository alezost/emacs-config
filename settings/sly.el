;;; sly.el --- Settings for `sly' package  -*- lexical-binding: t -*-

(require 'sly)
(require 'al-key)
(require 'al-clisp)
(require 'al-sly)

(defconst al/sly-prefix-keys
  '("C-d"
    ("C-z"   (al/sly 'other-window))
    ("C-j" . al/sly-switch-to-repl-and-enter)))

(defconst al/sly-xref-keys
  '(("." . sly-xref-prev-line)
    ("e" . sly-xref-next-line)
    ("u" . sly-xref-goto)
    ("d" . sly-xref-show)))
(al/bind-keys-from-vars 'sly-xref-mode-map 'al/sly-xref-keys)

(defconst al/sly-db-keys
  '(("."   . sly-db-up)
    ("e"   . sly-db-down)
    (">"   . sly-db-details-up)
    ("E"   . sly-db-details-down)
    ("M-." . sly-db-beginning-of-backtrace)
    ("M-e" . sly-db-end-of-backtrace)))
(al/bind-keys-from-vars 'sly-db-mode-map 'al/sly-db-keys)

(defconst al/sly-db-frame-keys
  '(([tab] sly-db-toggle-details)
    ("d"   sly-db-show-frame-source)
    ("v"   sly-db-eval-in-frame)))
(al/bind-keys-from-vars 'sly-db-frame-map
  '(al/sly-db-frame-keys al/button-keys))

(defconst al/sly-doc-keys
  '(("C-d" . sly-documentation-lookup)))
(al/bind-keys-from-vars 'sly-doc-map 'al/sly-doc-keys)

(defconst al/sly-repl-keys
  '(("C-c C-d" . al/sly-repl-disconnect-or-quit)
    ("M-r" . comint-history-isearch-backward-regexp)
    ("M-." . sly-mrepl-previous-input-or-button)
    ("M-e" . sly-mrepl-next-input-or-button)
    ("M->" . sly-mrepl-previous-prompt)
    ("M-E" . sly-mrepl-next-prompt)))
(al/bind-keys-from-vars 'sly-mrepl-mode-map 'al/sly-repl-keys)

;; `sly-editing-mode' is a useless wrapper for `sly-mode' but some
;; contrib modules add commands to its hook.  So making only `sly-mode'
;; work instead of `sly-editing-mode' would require too much
;; configuration.  At least, clean its keymap.
(al/clean-map 'sly-editing-mode-map)

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

;; Bind `sly-mode' keys after loading contribs because `sly-mrepl'
;; binds "C-c C-z".
(al/bind-keys-from-vars 'sly-prefix-map 'al/sly-prefix-keys)
(al/bind-keys-from-vars 'sly-mode-map 'al/sly-keys)

(al/clean-map 'sly-autodoc-mode-map)

(setq
 inferior-lisp-program "sbcl"
 sly--mode-line-format '(:eval (al/sly-mode-line-format)))

(advice-add 'sly-make-action-button
  :around #'al/sly-change-action-button-label)

;; Fix some indentation broken by `sly-cl-indent'.
(al/clisp-setup-indentation)

;;; sly.el ends here
