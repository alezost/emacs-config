;;; magit-popup.el --- Settings for `magit-popup' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros)
  (require 'al-key))

(require 'magit-popup)
(require 'al-general)
(require 'al-magit-popup)

(setq
 magit-popup-display-buffer-action '((display-buffer-at-bottom))
 magit-popup-show-common-commands nil
 magit-popup-use-prefix-argument 'default)

(al/bind-keys
  :map magit-popup-mode-map
  ("DEL" . al/magit-popup-previous-or-quit)
  ("M-." . backward-button)
  ("M-e" . forward-button)
  ("M-h" . magit-popup-toggle-show-common-commands))

(al/call-at-hook magit-popup-mode-hook al/bar-cursor-type)

;; Move away from buttons.  Adding `al/beginning-of-buffer' to
;; `magit-popup-mode-hook' wouldn't work because
;; `magit-refresh-popup-buffer' is called after the mode is set.
(advice-add 'magit-refresh-popup-buffer
  :after 'al/beginning-of-buffer)

;;; magit-popup.el ends here
