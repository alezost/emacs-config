;;; comint.el --- Settings for `comint' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'comint)
(require 'al-general)
(require 'al-key)

(setq
 comint-move-point-for-output nil
 comint-buffer-maximum-size 5000
 comint-password-prompt-regexp
 (rx-to-string `(or (and bol "Password")
                    (regex ,comint-password-prompt-regexp))))

(defconst al/comint-keys
  '(("M-." . comint-previous-input)
    ("M-e" . comint-next-input)
    ("M->" . comint-previous-prompt)
    ("M-E" . comint-next-prompt)
    ("C-c c" . compilation-shell-minor-mode)
    ("C-c o" . al/comint-toggle-move-point)
    ("C-c C-d" (process-send-eof))
    ("C-c C-k" . comint-kill-subjob)
    ("RET" . al/comint-send-input-maybe)
    "C-d")
  "Alist of auxiliary keys for comint modes.")
(al/bind-keys-from-vars 'comint-mode-map 'al/comint-keys)

(al/call-at-hook comint-mode-hook hl-todo-mode)
(add-hook 'comint-output-filter-functions #'comint-truncate-buffer)

;;; comint.el ends here
