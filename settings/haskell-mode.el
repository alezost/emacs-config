;;; haskell-mode.el --- Settings for `haskell-mode' package  -*- lexical-binding: t -*-

(require 'haskell-mode)
(require 'haskell-interactive-mode)
(require 'al-key)

(defconst al/haskell-general-keys
  '(("M-d" . haskell-mode-jump-to-def-or-tag)))

(defconst al/haskell-keys
  '(("C-c C-z" . haskell-interactive-switch)))
(al/bind-keys-from-vars 'haskell-mode-map
  '(al/haskell-general-keys al/haskell-keys))

(defconst al/haskell-interactive-keys
  '(("M-." . haskell-interactive-mode-history-previous)
    ("M-e" . haskell-interactive-mode-history-next)
    ("M->" . haskell-interactive-mode-prompt-previous)
    ("M-E" . haskell-interactive-mode-prompt-next)
    ("C-a" . haskell-interactive-mode-beginning)
    ("C-k" . haskell-interactive-mode-kill-whole-line)
    ("C-c C-d" (haskell-session-kill 'leave-buffer))))
(al/bind-keys-from-vars 'haskell-interactive-mode-map
  '(al/haskell-general-keys al/haskell-interactive-keys))

;;; haskell-mode.el ends here
