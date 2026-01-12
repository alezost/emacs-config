;;; hl-todo.el --- Highlight TODO and other keywords  -*- lexical-binding: t -*-

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

;;; Commentary:

;; This is a much simplified version of `hl-todo' package:
;; <https://github.com/tarsius/hl-todo>.  It just highlights
;; `hl-todo-keywords' and that's it.

;;; Code:

(defvar hl-todo-keywords '("TODO" "FIXME" "XXX" "WARNING" "ERROR")
  "List of words to highlight by `hl-todo-mode'.")

(defvar hl-todo-font-lock-keywords nil
  "List of keywords for `font-lock-add-keywords'.")

(defun hl-todo-setup ()
  "Set `hl-todo-font-lock-keywords' if needed."
  (unless hl-todo-font-lock-keywords
    (let ((re (concat "\\<\\("
                      (mapconcat #'identity hl-todo-keywords "\\|")
                      "\\)\\>")))
      (setq hl-todo-font-lock-keywords
            `((,re 1 'error prepend t))))))

;;;###autoload
(define-minor-mode hl-todo-mode
  "Highlight `hl-todo-keywords' in the current buffer."
  :lighter ""
  :group 'hl-todo
  (hl-todo-setup)
  (if hl-todo-mode
      (font-lock-add-keywords nil hl-todo-font-lock-keywords t)
    (font-lock-remove-keywords nil hl-todo-font-lock-keywords))
  (font-lock-flush))

(provide 'hl-todo)

;;; hl-todo.el ends here
