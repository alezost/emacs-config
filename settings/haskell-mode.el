;;; haskell-mode.el --- Settings for `haskell-mode' package  -*- lexical-binding: t -*-

(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'al-key)

(al/bind-keys
  :map al/haskell-general-map
  :create t
  ("M-d" 'haskell-mode-jump-to-def-or-tag))

(al/bind-keys
  :map haskell-mode-map
  :parent al/haskell-general-map
  ("C-c C-z" 'haskell-interactive-switch))

(al/bind-keys
  :map haskell-interactive-mode-map
  :parent al/haskell-general-map
  ("M-↑" 'haskell-interactive-mode-history-previous)
  ("M-↓" 'haskell-interactive-mode-history-next)
  ("M-S-↑" 'haskell-interactive-mode-prompt-previous)
  ("M-S-↓" 'haskell-interactive-mode-prompt-next)
  ("C-⇤" 'haskell-interactive-mode-beginning)
  ("C-k" 'haskell-interactive-mode-kill-whole-line)
  ("C-c C-d" (haskell-session-kill 'leave-buffer)))

;;; haskell-mode.el ends here
