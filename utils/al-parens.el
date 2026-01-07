;;; al-parens.el --- Additional functionality for working with parentheses  -*- lexical-binding: t -*-

;; Copyright © 2013–2026 Alex Kost

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


;;; Checking parentheses

(defvar al/check-parens-modes
  '(lisp-data-mode scheme-mode)
  "List of parent modes where `al/check-parens' is called.")

;;;###autoload
(defun al/check-parens ()
  "Run `check-parens' if `major-mode' derived from `al/check-parens-modes'."
  (interactive)
  (when (derived-mode-p al/check-parens-modes)
    (check-parens)))


;;; Commands for moving and editing

(declare-function sp-kill-sexp "smartparens")
(declare-function sp-backward-kill-sexp "smartparens")

;;;###autoload
(defun al/kill-sexp (&optional arg)
  "Kill sexp forward.
Similar to `kill-sexp', except if ARG is a raw prefix
\\[universal-argument], kill from point to the end of current
list/string, as `sp-kill-sexp' does."
  (interactive "P")
  (if (equal arg '(4))
      (progn (kill-sexp) (sp-kill-sexp arg))
    (kill-sexp (prefix-numeric-value arg))))

;;;###autoload
(defun al/backward-kill-sexp (&optional arg)
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
