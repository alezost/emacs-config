;;; tar-mode.el --- Settings for `tar-mode' package  -*- lexical-binding: t -*-

(require 'tar-mode)
(require 'al-key)

(al/bind-keys
  :map tar-mode-map
  ("↑" 'tar-previous-line)
  ("↓" 'tar-next-line)
  ("→" 'tar-extract))

(setq tar-mode-show-date t)

(add-hook 'tar-mode-hook #'hl-line-mode)

;;; tar-mode.el ends here
