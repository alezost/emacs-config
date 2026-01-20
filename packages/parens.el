;;; parens.el --- Additional functionality for working with parentheses  -*- lexical-binding: t -*-

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

(eval-when-compile (require 'cl-lib))


;;; Skipping parentheses

(defvar parens-string "()[]\"\""
  "String with parentheses skipped by `parens-skip'.")

(defun parens-skip (direction)
  "Skip parentheses at point and whitespaces after that.

If there are no parentheses at point, then skip whitespaces at first and
parentheses after that.

DIRECTION should be either `forward' or `backward' symbol.

Return non-nil, if something was skipped.  Return nil otherwise."
  (cl-flet ((skip (str)
              (abs (funcall (cl-ecase direction
                              (forward  #'skip-chars-forward)
                              (backward #'skip-chars-backward))
                            str))))
    (let* ((skipped1 (skip parens-string))
           (skipped2 (skip " \t\n"))
           (skipped3 (if (= 0 skipped1) (skip parens-string) 0))
           (skipped  (+ skipped1 skipped2 skipped3)))
      (> skipped 0))))

;;;###autoload
(defun parens-skip-forward ()
  "Skip parentheses at point forward and whitespaces after them.
See `parens-skip' for the returning value."
  (interactive)
  (parens-skip 'forward))

;;;###autoload
(defun parens-skip-backward ()
  "Skip parentheses at point backward and whitespaces before them.
See `parens-skip' for the returning value."
  (interactive)
  (parens-skip 'backward))


;;; Moving

(declare-function paredit-forward-up "paredit")
(declare-function paredit-forward-down "paredit")

;;;###autoload
(defun parens-forward-down* ()
  "Move forward down into a list.
This is similar to `paredit-forward-down' except if it is impossible to
move down, then move forward up and down again."
  (interactive)
  (condition-case nil
      (paredit-forward-down)
    (error
     (if (looking-at ")")
         (progn
           (paredit-forward-up)
           (parens-forward-down*))
       (message "Cannot move down")))))


;;; Editing

(declare-function sp-kill-sexp "smartparens")
(declare-function sp-backward-kill-sexp "smartparens")

;;;###autoload
(defun parens-kill-sexp-forward (&optional arg)
  "Kill sexp forward.
Similar to `kill-sexp', except if ARG is a raw prefix
\\[universal-argument], kill from point to the end of current
list/string, as `sp-kill-sexp' does."
  (interactive "P")
  (if (equal arg '(4))
      (progn (kill-sexp) (sp-kill-sexp arg))
    (kill-sexp (prefix-numeric-value arg))))

;;;###autoload
(defun parens-kill-sexp-backward (&optional arg)
  "Kill sexp backward.
Similar to `backward-kill-sexp', except if ARG is a raw prefix
\\[universal-argument], kill from point to the end of current
list/string, as `sp-backward-kill-sexp' does."
  (interactive "P")
  (if (equal arg '(4))
      (progn (backward-kill-sexp) (sp-backward-kill-sexp arg))
    (backward-kill-sexp (prefix-numeric-value arg))))

(provide 'parens)

;;; parens.el ends here
