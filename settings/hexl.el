;;; hexl.el --- Settings for `hexl' package  -*- lexical-binding: t -*-

(require 'hexl)
(require 'al-key)

(al/bind-keys
  :map hexl-mode-map
  ("C-↑" 'hexl-previous-line)
  ("C-↓" 'hexl-next-line)
  ("C-←" 'hexl-backward-char)
  ("C-→" 'hexl-forward-char)
  ("M-←" 'hexl-backward-short)
  ("M-→" 'hexl-forward-short)
  ("C-⇥" 'hexl-end-of-line)
  ("H-." 'hexl-scroll-down)
  ("H-e" 'hexl-scroll-up)
  ("H-a" 'hexl-beginning-of-buffer)
  ("H-i" 'hexl-end-of-buffer))

;;; hexl.el ends here
