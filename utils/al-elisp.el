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

(defun al/elisp-form-quoted-p (&rest _)
  "Replacement for `elisp--form-quoted-p'.
That function is used only by `elisp-completion-at-point' to define if
all types of symbols should be completed or only variables.
I always want to complete all symbols!"
  t)

(defvar al/elisp-feature-macros-regexp
  (rx "(" (group (or "al/with-eval-after-load"
                     "al/require"))
      symbol-end
      (one-or-more blank)
      (group (one-or-more (or (syntax word) (syntax symbol)))))
  "Regexp to match `al/with-eval-after-load' macro.")

(defun al/elisp-add-font-lock-keywords ()
  "Add `font-lock-keywords' to highlight additional macros.
Call this function once!"
  (font-lock-add-keywords
   'emacs-lisp-mode
   `((,al/elisp-feature-macros-regexp
      (1 font-lock-keyword-face)
      (2 font-lock-constant-face)))))

(provide 'al-elisp)

;;; al-elisp.el ends here
