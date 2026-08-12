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

(eval-when-compile
  (require 'al-aux-macros))
(require 'al-visual)


;;; Highlighting and indenting additional macros

(defun al/clisp-setup-indentation ()
  "Setup indentation for Common Lisp code."
  (al/put (common-lisp-indent-function
           sly-common-lisp-indent-function)
    (1 if
       if-let-
       if-let+
       if-let
       if-let1
       if-letn
       let-
       when-let-
       when-let+
       when-let
       when-let1
       when-letn
       defsystem
       define-configuration
       define-package
       al/define-keyscheme-map
       run-after-sleep)
    (3 defun-with-delay)))

(al/put doc-string-elt
  ;; My Lisp utils
  (4 defun-with-delay)
  ;; StumpWM
  (4 defcommand)
  ;; Nyxt
  (2 define-history-import-command
     define-bookmarklet-command
     define-bookmarklet-command-global)
  (3 define-mode
     define-command
     define-command-global
     define-command-prompt
     define-input-edit-command
     define-command-with-selection
     define-generic
     define-ffi-generic
     define-ffi-method
     defpsmacro
     define-parenscript
     define-parenscript-async)
  (4 define-panel-command
     define-panel-command-global
     define-internal-page
     define-internal-page-command
     define-internal-page-command-global))

(defvar al/clisp-keywords
  '("use-package"
    "defsystem"
    "define-package"
    "define-keyscheme-map"
    "al/define-keyscheme-map"
    "if-let-"
    "if-let+"
    "if-let"
    "if-let1"
    "if-letn"
    "let-"
    "when-let-"
    "when-let+"
    "when-let"
    "when-let1"
    "when-letn"
    "when-let*"
    "run-after-sleep")
  "List of additional keywords to highlight in `lisp-mode'.")

(defvar al/clisp-defcommand-regexp
  (rx "(" (group "defcommand")
      al/space
      (zero-or-one "(")
      (zero-or-one al/lisp-symbol-group))
  "Regexp to match `defcommand' StumpWM macro.")

(defvar al/clisp-defun-with-delay-regexp
  (rx "(" (group "defun-with-delay")
      al/space
      al/lisp-symbol-group
      al/space
      al/lisp-symbol-group)
  "Regexp to match `al/defun-with-delay' macro.")

(defvar al/clisp-define-class-regexp
  (rx "("
      (group (or "define-mode"
                 "define-class"
                 "define-configuration"))
      al/space
      al/lisp-symbol-group)
  "Regexp for Nyxt macros defining/modifying classes.")

(defvar al/clisp-define-other-regexp
  (rx "("
      (group (or "defpsmacro"
                 "define-parenscript"
                 "define-parenscript-async"
                 "define-generic"
                 "define-ffi-generic"
                 "define-ffi-method"
                 "define-command"
                 "define-command-global"
                 "define-command-prompt"
                 "define-input-edit-command"
                 "define-bookmarklet-command"
                 "define-bookmarklet-command-global"
                 "define-history-import-command"
                 "define-command-with-selection"
                 "define-panel-command"
                 "define-panel-command-global"
                 "define-internal-page"
                 "define-internal-page-command"
                 "define-internal-page-command-global"))
      al/space
      al/lisp-symbol-group)
  "Regexp for macros defining various things.")

(defun al/clisp-add-font-lock-keywords ()
  "Add `font-lock-keywords' to highlight additional macros.
Call this function once!"
  (al/add-simple-font-lock-keywords
   'lisp-mode al/clisp-keywords)
  (font-lock-add-keywords
   'lisp-mode
   `((,al/clisp-defun-with-delay-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face)
      (3 font-lock-function-name-face))
     (,al/clisp-defcommand-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face nil t))
     (,al/clisp-define-class-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-type-face))
     (,al/clisp-define-other-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face)))))


;;; Imenu entries

(declare-function al/add-to-imenu "al-imenu")

;;;###autoload
(defun al/clisp-imenu-add-definitions ()
  "Add defining entries to `imenu-generic-expression'."
  (al/add-to-imenu al/clisp-defcommand-regexp
                   :add-line-start t
                   :title "Commands"
                   :index 2)
  (al/add-to-imenu al/clisp-define-class-regexp
                   :add-line-start t
                   :title "Classes"
                   :index 2)
  (al/add-to-imenu al/clisp-define-other-regexp
                   :add-line-start t
                   :index 2))

(provide 'al-clisp)

;;; al-clisp.el ends here
