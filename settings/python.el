;;; python.el --- Settings for `python' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key))

(require 'python)
(require 'al-general)

(al/bind-keys
  :map python-mode-map
  ("C-v" 'python-shell-send-region)
  ("C-M-v" 'python-shell-send-defun)
  ("M-s-v" 'python-shell-send-buffer))

(setq python-shell-interpreter "ipython")

;;; python.el ends here
