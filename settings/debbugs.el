;;; debbugs.el --- Settings for `debbugs' package  -*- lexical-binding: t -*-

;; XXX `debbugs' package now contains `debbugs-guix'.

(require 'debbugs-gnu)
(require 'al-key)

(al/bind-keys
  :map debbugs-gnu-mode-map
  ("→" 'debbugs-gnu-select-report))

(setq debbugs-gnu-default-packages '("guix-patches"))

;;; debbugs.el ends here
