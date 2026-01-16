;;; al-scheme.el --- Additional functionality for `scheme-mode'  -*- lexical-binding: t -*-

;; Copyright © 2015–2026 Alex Kost

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

(require 'al-misc)


(defvar calculate-lisp-indent-last-sexp)

;; The following code of `al/scheme-indent-function' originates from
;; <http://www.netris.org/~mhw/scheme-indent-function.el>.

(defun al/scheme-indent-function (indent-point state)
  "Scheme mode function for the value of the variable `lisp-indent-function'.
This function is the same as `scheme-indent-function' except it
indents property lists properly and names starting with `default'."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
                                         calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
                                        (progn (forward-sexp 1) (point))))
            method)
        (setq method (or (get (intern-soft function) 'scheme-indent-function)
                         (get (intern-soft function) 'scheme-indent-hook)))
        (cond ((or (eq method 'defun)
                   (and (null method)
                        (> (length function) 3)
                        ;; The original regexp is "\\`def" but it will
                        ;; mess indentation with such names as
                        ;; 'default-...'.
                        (string-match "\\`define" function)))
               (lisp-indent-defform state indent-point))
              ;; This next cond clause is the only change -mhw
              ((and (null method)
                    (> (length function) 1)
                    ;; The '#' in '#:' seems to get lost, not sure why
                    (string-match "\\`:" function))
               (let ((lisp-body-indent 1))
                 (lisp-indent-defform state indent-point)))
              ((integerp method)
               (lisp-indent-specform method state
                                     indent-point normal-indent))
              (method
               (funcall method state indent-point normal-indent)))))))


;;; Docstrings highlighting

;; Although `scheme-mode' has all the functionality to highlight
;; docstrings properly (with `font-lock-doc-face', not with
;; `font-lock-string-face'!), it doesn't do it.  The only missing thing
;; needed to fix it, is setting `font-lock-syntactic-face-function'.

;; XXX This is definitely an Emacs bug, and I should report about it!  I
;; think it was introduced by
;; <http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=0a5cfeeecb9e1038f9df3b34b61b797e56213a7b>.

;; Another miss is that "scheme.el" contains "docstring rules" only for
;; `define' and `lambda*', while there are other things to highlight:

(al/put scheme-doc-string-elt
  (1 case-lambda
     case-lambda*)
  (2 define*
     lambda*
     define-syntax-rule
     syntax-rules))

(defun al/scheme-fix-docstring-font-lock ()
  "Fix highlighting of the Scheme docstrings.
This function is intended to be added to `scheme-mode-hook'."
  (setq-local font-lock-syntactic-face-function
              'lisp-font-lock-syntactic-face-function))


;;; Highlighting and indenting additional macros

(al/put scheme-indent-function
  (0 case-lambda*
     and-let)
  (1 if
     if-let
     when-let))

(defvar al/scheme-keywords
  '("push!"
    "define-delayed"
    "define-osd"
    "case-lambda*"
    "and-let"
    "if-let"
    "when-let")
  "List of additional keywords to highlight in `scheme-mode'.")

(defun al/scheme-add-font-lock-keywords ()
  "Add `font-lock-keywords' to highlight additional macros.
Call this function once!"
  (al/add-simple-font-lock-keywords
   'scheme-mode al/scheme-keywords))

(provide 'al-scheme)

;;; al-scheme.el ends here
