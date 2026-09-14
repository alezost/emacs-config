;;; hexl.el --- Settings for `hexl' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'hexl)

(al/bind-keys
  :map hexl-mode-map
  ("C-↑" 'hexl-previous-line)
  ("C-↓" 'hexl-next-line)
  ("C-←" 'hexl-backward-char)
  ("C-→" 'hexl-forward-char)
  ("M-←" 'hexl-backward-short)
  ("M-→" 'hexl-forward-short)
  ("C-⇥" 'hexl-end-of-line)
  ("H-↑" 'hexl-scroll-down)
  ("H-↓" 'hexl-scroll-up)
  ("H-⇤" 'hexl-beginning-of-buffer)
  ("H-⇥" 'hexl-end-of-buffer))

;;; hexl.el ends here
