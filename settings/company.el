;;; company.el --- Settings for `company' package  -*- lexical-binding: t -*-

(require 'company)
(require 'al-key)

(setq
 company-idle-delay nil
 company-show-quick-access t)

(al/bind-keys
  :map company-active-map
  ("C-↑" company-select-previous)
  ("C-↓" company-select-next)
  ("M-↑" company-select-previous)
  ("M-↓" company-select-next))

(global-company-mode)

;;; company.el ends here
