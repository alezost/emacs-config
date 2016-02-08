;;; al-parens.el --- Additional functionality for working with parentheses

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 29 Sep 2015

;;; Code:

(require 'smartparens)

;;;###autoload
(defun utl-kill-sexp (&optional arg)
  "Kill sexp forward.
Similar to `kill-sexp', except if ARG is a raw prefix
\\[universal-argument], kill from point to the end of current
list/string, as `sp-kill-sexp' does."
  (interactive "P")
  (if (equal arg '(4))
      (progn (kill-sexp) (sp-kill-sexp arg))
    (kill-sexp (prefix-numeric-value arg))))

;;;###autoload
(defun utl-backward-kill-sexp (&optional arg)
  "Kill sexp backward.
Similar to `backward-kill-sexp', except if ARG is a raw prefix
\\[universal-argument], kill from point to the end of current
list/string, as `sp-backward-kill-sexp' does."
  (interactive "P")
  (if (equal arg '(4))
      (progn (backward-kill-sexp) (sp-backward-kill-sexp arg))
    (backward-kill-sexp (prefix-numeric-value arg))))

(provide 'al-parens)

;;; al-parens.el ends here
