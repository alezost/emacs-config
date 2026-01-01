;;; al-lisp.el --- Additional functionality for `lisp-mode'  -*- lexical-binding: t -*-

;; Copyright © 2017–2026 Alex Kost

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

(require 'al-imenu)

(defun al/put-lisp-indent (symbol value)
  "Set SYMBOL's lisp indent property to VALUE."
  (put symbol 'lisp-indent-function value)
  (put symbol 'common-lisp-indent-function value))


;;; Highlighting and indenting additional macros

(al/put-lisp-indent 'defsystem 1)
(al/put-lisp-indent 'al/defun-with-delay 3)
(al/put-lisp-indent 'al/run-after-sleep 1)
(put 'defcommand 'doc-string-elt 4)
(put 'al/defun-with-delay 'doc-string-elt 4)

(defvar al/lisp-macro-name-regexp
  (rx "(" (group (or "if-let" "when-let" "when-let*"
                     "defsystem" "al/run-after-sleep"))
      symbol-end)
  "Regexp to match macro names to highlight.")

(defvar al/lisp-defcommand-regexp
  (rx line-start
      "(" (group "defcommand")
      symbol-end
      (zero-or-more blank)
      (zero-or-one "(")
      (zero-or-one
       (group (one-or-more (or (syntax word) (syntax symbol))))))
  "Regexp to match `defcommand' keyword.")

(defvar al/lisp-defun-with-delay-regexp
  (rx "(" (group "al/defun-with-delay")
      symbol-end
      (one-or-more blank)
      (group (one-or-more (or (syntax word) (syntax symbol))))
      (one-or-more blank)
      (group (one-or-more (or (syntax word) (syntax symbol)))))
  "Regexp to match `al/defun-with-delay' macro.")

(defun al/lisp-add-font-lock-keywords ()
  "Add font-lock keywords to highlight additional macros.
Call this function once!"
  (font-lock-add-keywords
   'lisp-mode
   `((,al/lisp-macro-name-regexp 1 font-lock-keyword-face)
     (,al/lisp-defcommand-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     (,al/lisp-defun-with-delay-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face)
      (3 font-lock-function-name-face)))))

(defun al/lisp-add-defcommand-to-imenu ()
  "Add `defcommand' entries to `imenu-generic-expression'.
This function is intended to be added to `lisp-mode-hook'."
  (al/add-to-imenu al/lisp-defcommand-regexp
                   :index 2))

(provide 'al-lisp)

;;; al-lisp.el ends here
