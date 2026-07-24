;;; al-clisp.el --- Additional functionality for `lisp-mode'  -*- lexical-binding: t -*-

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

(require 'al-general)
(require 'al-visual)


;;; Highlighting and indenting additional macros

(defun al/clisp-setup-indentation ()
  "Setup indentation for Common Lisp code."
  (al/put (common-lisp-indent-function
           sly-common-lisp-indent-function)
    (1 if
       defsystem
       al/run-after-sleep)
    (3 al/defun-with-delay)))

(al/put doc-string-elt
  (4 defcommand
     al/defun-with-delay))

(defvar al/clisp-keywords
  '("if-let"
    "when-let"
    "when-let*"
    "defsystem"
    "al/run-after-sleep")
  "List of additional keywords to highlight in `lisp-mode'.")

(defvar al/clisp-defcommand-regexp
  (rx "(" (group "defcommand")
      al/space
      (zero-or-one "(")
      (zero-or-one al/lisp-symbol-group))
  "Regexp to match `defcommand' StumpWM macro.")

(defvar al/clisp-defun-with-delay-regexp
  (rx "(" (group "al/defun-with-delay")
      al/space
      al/lisp-symbol-group
      al/space
      al/lisp-symbol-group)
  "Regexp to match `al/defun-with-delay' macro.")

(defun al/clisp-add-font-lock-keywords ()
  "Add `font-lock-keywords' to highlight additional macros.
Call this function once!"
  (al/add-simple-font-lock-keywords
   'lisp-mode al/clisp-keywords)
  (font-lock-add-keywords
   'lisp-mode
   `((,al/clisp-defcommand-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     (,al/clisp-defun-with-delay-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face)
      (3 font-lock-function-name-face)))))

(declare-function al/add-to-imenu "al-imenu")

;;;###autoload
(defun al/clisp-imenu-add-defcommand ()
  "Add `defcommand' entries to `imenu-generic-expression'."
  (al/add-to-imenu al/clisp-defcommand-regexp
                   :add-line-start t
                   :title "Commands"
                   :index 2))

(provide 'al-clisp)

;;; al-clisp.el ends here
