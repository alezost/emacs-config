;;; debbugs.el --- Settings for `debbugs' package  -*- lexical-binding: t -*-

;; XXX `debbugs' package now contains `debbugs-guix'.

(require 'debbugs-gnu)
(require 'al-key)

(defconst al/debbugs-gnu-keys
  '(("u" . debbugs-gnu-select-report))
  "Alist of auxiliary keys for `debbugs-gnu-mode-map'.")
(al/bind-keys-from-vars 'debbugs-gnu-mode-map 'al/debbugs-gnu-keys)

(setq debbugs-gnu-default-packages '("guix-patches"))

;;; debbugs.el ends here
