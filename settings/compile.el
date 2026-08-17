;;; compile.el --- Settings for `compile' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'compile)
(require 'al-compilation)
(require 'al-places)
(require 'al-key)

(defconst al/compilation-common-keys
  '(("C-M-h" . compilation-previous-error)
    ("C-M-n" . compilation-next-error)
    ("C-M-." . compilation-previous-error)
    ("C-M-e" . compilation-next-error)))

(defconst al/compilation-keys
  '(("."   . compilation-previous-error)
    ("e"   . compilation-next-error)
    ("M-." . previous-error-no-select)
    ("M-e" . next-error-no-select)))

(defconst al/compilation-button-keys
  '(("u"   . compile-goto-error)))

(al/bind-keys-from-vars 'compilation-button-map
  'al/compilation-button-keys)
(al/bind-keys-from-vars 'compilation-shell-minor-mode-map
  'al/compilation-common-keys)
(al/bind-keys-from-vars
    '(compilation-mode-map compilation-minor-mode-map)
  '(al/compilation-common-keys al/compilation-keys))

(setq
 ;; Don't ask, don't save.
 compilation-ask-about-save nil
 compilation-save-buffers-predicate 'ignore)

(al/setq-file
 al/compilation-sound-success (al/sound-dir-file "bell.oga")
 al/compilation-sound-error   (al/sound-dir-file "splat.wav"))

(al/call-at-hook compilation-mode-hook al/hl-line-mode)

(add-hook 'compilation-finish-functions #'al/compilation-notify)

;;; compile.el ends here
