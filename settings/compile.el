;;; compile.el --- Settings for `compile' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'compile)
(require 'al-compilation)
(require 'al-places)
(require 'al-key)

(al/bind-keys
  :map al/compilation-common-map
  :create t
  ("C-M-h" 'compilation-previous-error)
  ("C-M-n" 'compilation-next-error)
  ("C-M-↑" 'compilation-previous-error)
  ("C-M-↓" 'compilation-next-error))

(al/bind-keys
  :map compilation-button-map
  ("→" 'compile-goto-error))

(al/bind-keys
  :map compilation-shell-minor-mode-map
  :parent al/compilation-common-map)

(al/bind-keys
  :map compilation-minor-mode-map
  :parent (al/compilation-common-map
           special-mode-map)
  ("↑"   'compilation-previous-error)
  ("↓"   'compilation-next-error)
  ("M-↑" 'previous-error-no-select)
  ("M-↓" 'next-error-no-select))

(al/bind-keys
  :map compilation-mode-map
  :parent compilation-minor-mode-map)

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
