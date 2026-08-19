;;; company.el --- Settings for `company' package  -*- lexical-binding: t -*-

(require 'company)
(require 'al-key)

(setq
 company-idle-delay nil
 company-show-quick-access t)

(defconst al/company-active-map
  '(("C-." . company-select-previous)
    ("C-e" . company-select-next)
    ("M-." . company-select-previous)
    ("M-e" . company-select-next))
  "Alist of auxiliary keys for `company-active-map'.")

(al/bind-keys-from-vars 'company-active-map 'al/company-active-map)

(global-company-mode)

;;; company.el ends here
