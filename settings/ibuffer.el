;;; ibuffer.el --- Settings for `ibuffer' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'ibuffer)
(require 'al-general)
(require 'al-key)
(require 'al-visual)

(al/bind-keys
  :map ibuffer-mode-map
  ("→"   'ibuffer-visit-buffer)
  ("↑"   'ibuffer-backward-line)
  ("↓"   'ibuffer-forward-line)
  ("M-↑" 'ibuffer-backward-filter-group)
  ("M-↓" 'ibuffer-forward-filter-group)

  ("d"   'ibuffer-visit-buffer-other-window-noselect)
  ("C-d" 'ibuffer-visit-buffer-other-window)
  ("C-l" (ibuffer-update t))

  ("M"   'ibuffer-mark-unsaved-buffers)
  ("z"   'ibuffer-unmark-forward)
  ("Z"   (ibuffer-unmark-all ?\r))
  ("s r" 'ibuffer-do-sort-by-recency)
  ("* o" 'ibuffer-mark-old-buffers))

(setq ibuffer-default-sorting-mode 'recency)

(al/eval-at-hook ibuffer-mode-hook
  (hl-line-mode)
  (setq al/mode-info
        '(""
          (ibuffer-sorting-mode (:eval (symbol-name ibuffer-sorting-mode)))
          (ibuffer-sorting-reversep "|r"))))

;;; ibuffer.el ends here
