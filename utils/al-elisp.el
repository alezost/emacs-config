;;; al-elisp.el --- Additional functionality for elisp, eldoc

;; Copyright © 2013–2016, 2021 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'pp)

;;;###autoload
(defun al/pp-eval-expression (expression)
  "Same as `pp-eval-expression' but without \"Evaluating...\" message."
  (interactive
   (list (read--expression "Eval: ")))
  (setq values (cons (eval expression) values))
  (pp-display-expression (car values) "*Pp Eval Output*"))

;;;###autoload
(defun al/eval-dwim (arg)
  "Eval last sexp or region if it is active.
ARG is passed to `eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (progn
        (eval-region (region-beginning) (region-end))
        (deactivate-mark))
    (eval-last-sexp arg)))

;;;###autoload
(defun al/pp-eval-dwim (arg)
  "Eval last sexp or region if it is active.
ARG is passed to `pp-eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (progn
        (eval-region (region-beginning) (region-end))
        (deactivate-mark))
    (pp-eval-last-sexp arg)))

;;;###autoload
(defun al/indent-sexp (&optional no-offset pp)
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
