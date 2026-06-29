;;; al-elisp.el --- Additional functionality for elisp  -*- lexical-binding: t -*-

;; Copyright © 2025–2026 Alex Kost

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

(require 'al-visual)

(defun al/elisp-form-quoted-p (&rest _)
  "Replacement for `elisp--form-quoted-p'.
That function is used only by `elisp-completion-at-point' to define if
all types of symbols should be completed or only variables.
I always want to complete all symbols!"
  t)


;;; Highlighting and indenting additional macros

(al/put doc-string-elt
  (2 al/defun-lazy))

(defvar al/elisp-keywords
  '("with-no-warnings"
    "define-button-type")
  "List of additional keywords to highlight in `elisp-mode'.
Usually, these are functions that behave like macros.")

(defvar al/elisp-feature-macros-regexp
  (rx "(" (group (or "al/eval-after-load"
                     "al/require"))
      symbol-end
      (one-or-more blank)
      (group (one-or-more (or (syntax word) (syntax symbol)))))
  "Regexp to match `al/eval-after-load' macro.")

(defvar al/elisp-defun-lazy-regexp
  (rx "(" (group "al/defun-lazy")
      symbol-end
      (one-or-more blank)
      (group (one-or-more (or (syntax word) (syntax symbol)))))
  "Regexp to match `al/defun-lazy' macro.")

(defun al/elisp-add-font-lock-keywords ()
  "Add `font-lock-keywords' to highlight additional macros.
Call this function once!"
  (al/add-simple-font-lock-keywords
   'emacs-lisp-mode al/elisp-keywords)
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,al/elisp-defun-lazy-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-function-name-face))
     (,al/elisp-feature-macros-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face)))))


;;; Imenu entries

(declare-function al/add-to-imenu "al-imenu")

;;;###autoload
(defun al/elisp-imenu-add-defun-lazy ()
  "Add `al/elisp-defun-lazy-regexp' to `imenu-generic-expression'."
  (al/add-to-imenu al/elisp-defun-lazy-regexp :index 2))

;;; `use-package' entries

;; Idea from <https://github.com/jwiegley/use-package/issues/80>.

(defvar al/elisp-imenu-use-package-re
  (rx bol "(use-package" (+ whitespace)
      (? ?\")
      (group (+ (or (syntax word) (syntax symbol))))
      (? ?\"))
  "Regexp for `use-package' entries in imenu.")

(defvar al/elisp-imenu-use-package-group "use-package"
  "Group name in imenu index of use-package entries.
If nil, put the entries in a top level.  See MENU-TITLE in
`imenu-generic-expression' variable for details.")

;;;###autoload
(defun al/elisp-imenu-add-use-package ()
  "Add `al/elisp-imenu-use-package-re' to `imenu-generic-expression'."
  (al/add-to-imenu al/elisp-imenu-use-package-re
                   :title al/elisp-imenu-use-package-group))

;;; (with-)eval-after-load entries

(defvar al/elisp-imenu-eval-after-load-re
  (rx bol "(" (zero-or-one (or "al/" "with-"))
      "eval-after-load" (+ whitespace)
      (zero-or-one (or ?\" ?'))
      (group (+ (or (syntax word) (syntax symbol))))
      (zero-or-one ?\"))
  "Regexp for `eval-after-load' and `with-eval-after-load' entries in imenu.")

(defvar al/elisp-imenu-eval-after-load-group "(with-)eval-after-load")

;;;###autoload
(defun al/elisp-imenu-add-eval-after-load ()
  "Add `al/elisp-imenu-eval-after-load-re' to `imenu-generic-expression'."
  (al/add-to-imenu al/elisp-imenu-eval-after-load-re
                   :title al/elisp-imenu-eval-after-load-group))

;;; Transient entries

(defvar al/elisp-imenu-transient-re
  (rx bol "(transient-define-" (+ (or (syntax word) (syntax symbol)))
      (+ whitespace)
      (group (+ (or (syntax word) (syntax symbol)))))
  "Regexp for transient entries in imenu.")

(defvar al/elisp-imenu-transient-group "transient")

;;;###autoload
(defun al/elisp-imenu-add-transient ()
  "Add `al/elisp-imenu-transient-re' to `imenu-generic-expression'."
  (al/add-to-imenu al/elisp-imenu-transient-re
                   :title al/elisp-imenu-transient-group))

(provide 'al-elisp)

;;; al-elisp.el ends here
