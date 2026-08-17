;;; imenu.el --- Settings for `imenu' package  -*- lexical-binding: t -*-

(require 'imenu)
(require 'al-imenu)

(setq
 ;; imenu-flatten t
 imenu-space-replacement nil
 imenu-level-separator " ⇨ "
 al/imenu-mode-alist
 '((lisp-data-mode  al/lisp-imenu-add-sections)
   (emacs-lisp-mode al/elisp-imenu-add-defun
                    al/elisp-imenu-add-use-package
                    al/elisp-imenu-add-transient
                    al/elisp-imenu-add-eval-after-load)
   (lisp-mode       al/clisp-imenu-add-definitions)
   (scheme-mode     al/lisp-imenu-add-sections
                    al/scheme-imenu-add-define-values)
   (js-mode         al/js-imenu-add-sections)))

(advice-add 'imenu--make-index-alist :before #'al/imenu-augment)

;;; imenu.el ends here
