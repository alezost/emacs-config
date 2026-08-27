;;; transient.el --- Settings for `transient' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'transient)
(require 'al-places)
(require 'al-general)
(require 'al-key)
(require 'al-transient)

(al/bind-keys
  :map transient-base-map
  "C-v" "M-v"
  ;; Don't bind "q" because transient will quit even for complex
  ;; bindings such as "-q"!
  ;;
  ;; ("q" 'transient-quit-all)
  ("C-g" 'transient-quit-all)
  ("C-q" 'transient-quit-one)
  ("DEL" 'transient-quit-one))

(al/bind-keys
  :map transient-sticky-map
  ("C-g" 'transient-quit-all)
  ("C-q" 'transient-quit-seq))

(al/bind-keys
  :map transient-map
  ("C-M-↶" 'transient-history-next)
  ("C-M-↷" 'transient-history-prev))

(al/bind-keys
  :map transient-popup-navigation-map
  ("<tab>" 'transient-forward-button)
  ("<backtab>" 'transient-backward-button)
  ("C-↑"   'transient-backward-button)
  ("C-↓"   'transient-forward-button))

(transient-suffix-put 'transient-common-commands
                      "C-g" :command 'transient-quit-all)
(transient-suffix-put 'transient-common-commands
                      "C-q" :command 'transient-quit-one)

(setq
 transient-levels-file  (al/emacs-data-dir-file "transient/levels.el")
 transient-history-file (al/emacs-data-dir-file "transient/history.el")
 transient-values-file  (al/emacs-data-dir-file "transient/values.el")
 transient--buffer-name "*transient*"
 ;; transient-detect-key-conflicts t
 ;; transient--debug t
 transient-highlight-mismatched-keys nil
 transient-enable-popup-navigation nil
 transient-read-with-initial-input nil
 transient-mode-line-format mode-line-format)

(advice-add 'transient-setup :before #'al/transient-fix-input-method)

(al/call-at-hook transient-post-exit-hook
  al/transient-restore-input-method)

;;; transient.el ends here
