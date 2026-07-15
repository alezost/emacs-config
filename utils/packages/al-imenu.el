;;; al-imenu.el --- Additional functionality for `imenu' package  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

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

(cl-defun al/add-to-imenu (regexp &key (index 1) title append add-line-start)
  "Add REGEXP with INDEX and TITLE to `imenu-generic-expression'.
If APPEND is non-nil, add the new element at the end.
If ADD-LINE-START is non-nil, add line-start to REGEXP."
  (add-to-list 'imenu-generic-expression
               (list title
                     (if add-line-start
                         (concat "^" regexp)
                       regexp)
                     index)
               append))

(defvar al/imenu-mode-alist nil
  "Alist of major modes and functions adding imenu expressions.
Each element has (MODE FUNCTIONS ...) form.  When `imenu' is called for
the first time in a buffer with `major-mode' derived from MODE, it
evaluates FUNCTIONS which are supposed to add new elements to
`imenu-generic-expression'.")

(defvar-local al/imenu-augmented nil
  "If non-nil, `al/imenu-augment' is already called for this buffer.")

(defun al/imenu-augment (&rest _)
  "Augment `imenu' using `al/imenu-mode-alist'."
  (unless al/imenu-augmented
    (pcase-dolist (`(,mode . ,funs) al/imenu-mode-alist)
      (when (derived-mode-p mode)
        (dolist (fun funs)
          (funcall fun))))
    (setq al/imenu-augmented t)))

(provide 'al-imenu)

;;; al-imenu.el ends here
