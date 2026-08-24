;;; agent-shell.el --- Settings for `agent-shell' package  -*- lexical-binding: t -*-

(require 'agent-shell)
(require 'al-key)

(al/bind-keys
  :map agent-shell-mode-map
  ("RET" shell-maker-submit)
  ([tab] al/agent-next-item-or-complete))

(setq
 agent-shell-preferred-agent-config 'qwen-code)

;; `agent-shell--trigger-completion-at-point' is added to
;; `post-self-insert-hook' by `agent-shell-completion-mode'.  It calls
;; `completion-at-point' immediately after "/" is written.  I don't need
;; this shit.
(advice-add 'agent-shell--trigger-completion-at-point :override #'ignore)

;;; agent-shell.el ends here
