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

(require 'al-misc)
(require 'al-imenu)


;;; Highlighting and indenting additional macros

(al/put (lisp-indent-function
         common-lisp-indent-function)
  (1 defsystem
     al/run-after-sleep)
  (3 al/defun-with-delay))

(al/put doc-string-elt
  (4 defcommand
     al/defun-with-delay))

(defvar al/lisp-keywords
  '("if-let"
    "when-let"
    "when-let*"
    "defsystem"
    "al/run-after-sleep")
  "List of additional keywords to highlight in `lisp-mode'.")

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
  "Add `font-lock-keywords' to highlight additional macros.
Call this function once!"
  (al/add-simple-font-lock-keywords
   'lisp-mode al/lisp-keywords)
  (font-lock-add-keywords
   'lisp-mode
   `((,al/lisp-defcommand-regexp
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
