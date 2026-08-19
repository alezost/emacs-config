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
;; than `eshell-mode' does.
(defconst al/ediff-keys
  '(("h" . ediff-previous-difference)
    ("H" . ediff-toggle-hilit))
  "Alist of auxiliary keys for `ediff-mode-map'.")

(al/eval-at-hook ediff-startup-hook
  (al/bind-keys-from-vars 'ediff-mode-map 'al/ediff-keys))

(al/call-at-hook ediff-before-setup-hook
  al/ediff-save-window-configuration)

(al/call-at-hook ediff-quit-hook
  :depth 100
  al/ediff-restore-window-configuration)

;;; ediff.el ends here
