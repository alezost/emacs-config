;;; xref.el --- Settings for `xref' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key))

(require 'xref)
(require 'al-general)

(al/bind-keys
  :map xref--xref-buffer-mode-map
  ("↑" 'xref-prev-line)
  ("↓" 'xref-next-line)
  ("→" 'xref-goto-xref)
  ("d" 'xref-show-location-at-point))

(setq xref-backend-functions '(elisp--xref-backend))

;;; xref.el ends here
