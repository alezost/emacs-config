;;; al-elisp.el --- Additional functionality for elisp, eldoc

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 12 Sep 2013

;;; Code:

(require 'pp)

;;;###autoload
(defun utl-pp-eval-expression (expression)
  "Same as `pp-eval-expression' but without \"Evaluating...\" message."
  (interactive
   (list (read--expression "Eval: ")))
  (setq values (cons (eval expression) values))
  (pp-display-expression (car values) "*Pp Eval Output*"))

;;;###autoload
(defun utl-eval-dwim (arg)
  "Eval last sexp or region if it is active.
ARG is passed to `eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (eval-region (region-beginning) (region-end))
    (eval-last-sexp arg)))

;;;###autoload
(defun utl-pp-eval-dwim (arg)
  "Eval last sexp or region if it is active.
ARG is passed to `pp-eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (eval-region (region-beginning) (region-end))
    (pp-eval-last-sexp arg)))

;; From <http://www.emacswiki.org/emacs/ElDoc>.
;;;###autoload
(defun utl-eldoc-argument-list (string)
  "Fontify STRING for use with `eldoc-mode'.
This function is suitable for `eldoc-argument-case' variable."
  (propertize (upcase string)
              'face 'font-lock-variable-name-face))

;;;###autoload
(defun utl-indent-sexp (&optional no-offset pp)
  "Indent each line of the list starting just after point.
If NO-OFFSET is non-nil (with \\[universal-argument]), indent
without offset for the following lines.
If PP is non-nil (with \\[universal-argument] \\[universal-argument]), pretty-print the following list."
  (interactive
   (list (equal current-prefix-arg '(4))
         (equal current-prefix-arg '(16))))
  (let ((lisp-indent-offset (and no-offset 1)))
    (indent-pp-sexp pp)))

(provide 'al-elisp)

;;; al-elisp.el ends here
