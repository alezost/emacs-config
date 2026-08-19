;;; pcomplete.el --- Settings for `pcomplete' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'pcomplete)

(with-suppressed-warnings ((obsolete pcomplete-suffix-list))
  ;; Although `pcomplete-suffix-list' is marked as obsolete, it is used
  ;; by `pcomplete-insert-entry', and its default value prevents
  ;; inserting space after ":" (while completing ERC nicks).
  (setq pcomplete-suffix-list nil))

(al/eval-at-hook (shell-mode-hook eshell-mode-hook)
  (setq-local pcomplete-termination-string ""))

;;; pcomplete.el ends here
