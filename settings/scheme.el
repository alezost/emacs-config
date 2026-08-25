;;; scheme.el --- Settings for `scheme' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'scheme)
(require 'al-scheme)
(require 'al-key)

(al/modify-page-break-syntax scheme-mode-syntax-table)

(al/call-at-hook scheme-mode-hook
  guix-devel-mode
  al/scheme-fix-docstring-font-lock
  al/scheme-fix-fill)

(al/scheme-add-font-lock-keywords)

(advice-add 'scheme-indent-function
  :override #'al/scheme-indent-function)

(al/eval-after-load xscheme
  ;; I don't how this `xscheme' package is loaded from time to time but
  ;; it pollutes `scheme-mode-map' (in particular, it breaks my "M-o"
  ;; key binding).
  (al/clean-keymap scheme-mode-map))

;;; scheme.el ends here
