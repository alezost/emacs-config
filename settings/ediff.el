;;; ediff.el --- Settings for `ediff' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'ediff)
(require 'al-key)
(require 'al-ediff)

(setq
 ediff-window-setup-function #'ediff-setup-windows-plain ; no new frame
 ediff-split-window-function #'split-window-horizontally
 ediff-grab-mouse nil)

;; The way `ediff-mode' works with the key bindings is even more evil
;; than `eshell-mode' does: instead of making several global keymap
;; variables with properly configured parents, they reset
;; `ediff-mode-map' with all the keybindings for each new ediff session
;; (see `ediff-setup-keymap').
(al/eval-at-hook ediff-startup-hook
  (al/bind-keys
    :map ediff-mode-map
    ("h" 'ediff-previous-difference)
    ("H" 'ediff-toggle-hilit)))

(al/call-at-hook ediff-before-setup-hook
  al/ediff-save-window-configuration)

(al/call-at-hook ediff-quit-hook
  :depth 100
  al/ediff-restore-window-configuration)

;;; ediff.el ends here
