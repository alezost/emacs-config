;;; hexl.el --- Settings for `hexl' package  -*- lexical-binding: t -*-

(require 'hexl)
(require 'al-key)

(al/bind-keys
  :map hexl-mode-map
  ("C-." . hexl-previous-line)
  ("C-e" . hexl-next-line)
  ("C-o" . hexl-backward-char)
  ("C-u" . hexl-forward-char)
  ("M-o" . hexl-backward-short)
  ("M-u" . hexl-forward-short)
  ("C-i" . hexl-end-of-line)
  ("H-." . hexl-scroll-down)
  ("H-e" . hexl-scroll-up)
  ("H-a" . hexl-beginning-of-buffer)
  ("H-i" . hexl-end-of-buffer))

;;; hexl.el ends here
