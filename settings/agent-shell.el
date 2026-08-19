;;; agent-shell.el --- Settings for `agent-shell' package  -*- lexical-binding: t -*-

(require 'agent-shell)
(require 'al-key)

(defconst al/agent-shell-keys
  '(("RET" . shell-maker-submit)
    ("TAB" . al/agent-next-item-or-complete))
  "Alist of auxiliary keys for `agent-shell-mode-map'.")
(al/bind-keys-from-vars 'agent-shell-mode-map 'al/agent-shell-keys t)

(setq
 agent-shell-preferred-agent-config 'qwen-code)

;; `agent-shell--trigger-completion-at-point' is added to
;; `post-self-insert-hook' by `agent-shell-completion-mode'.  It calls
;; `completion-at-point' immediately after "/" is written.  I don't need
;; this shit.
(advice-add 'agent-shell--trigger-completion-at-point :override #'ignore)

;;; agent-shell.el ends here
