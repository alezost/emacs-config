;;; transient.el --- Settings for `transient' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'transient)
(require 'al-places)
(require 'al-general)
(require 'al-key)
(require 'al-transient)

(defconst al/transient-base-keys
  '("C-v" "M-v"
    ;; Don't bind "q" because transient will quit even for complex
    ;; bindings such as "-q"!
    ;;
    ;; ("q" . transient-quit-all)
    ("C-g" . transient-quit-all)
    ("C-q" . transient-quit-one)
    ("DEL" . transient-quit-one))
  "Alist of auxiliary keys for `transient-base-map'.")
(al/bind-keys-from-vars 'transient-base-map 'al/transient-base-keys)

(defconst al/transient-sticky-keys
  '(("C-g" . transient-quit-all)
    ("C-q" . transient-quit-seq))
  "Alist of auxiliary keys for `transient-sticky-map'.")
(al/bind-keys-from-vars 'transient-sticky-map 'al/transient-sticky-keys)

(defconst al/transient-keys
  '(("C-M-p" . transient-history-next)
    ("C-M-," . transient-history-prev))
  "Alist of auxiliary keys for `transient-map'.")
(al/bind-keys-from-vars 'transient-map 'al/transient-keys t)

(defconst al/transient-navigation-keys
  '(("<tab>" . transient-forward-button)
    ("<backtab>" . transient-backward-button)
    ("C-."   . transient-backward-button)
    ("C-e"   . transient-forward-button))
  "Alist of auxiliary keys for `transient-popup-navigation-map'.")
(al/bind-keys-from-vars 'transient-popup-navigation-map
  'al/transient-navigation-keys)

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
