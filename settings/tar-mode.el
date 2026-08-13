;;; tar-mode.el --- Settings for `tar-mode' package  -*- lexical-binding: t -*-

(require 'tar-mode)
(require 'al-key)

(setq tar-mode-show-date t)

(al/bind-keys
  :map tar-mode-map
  ("." . tar-previous-line)
  ("e" . tar-next-line)
  ("u" . tar-extract))

(add-hook 'tar-mode-hook #'hl-line-mode)

;;; tar-mode.el ends here
