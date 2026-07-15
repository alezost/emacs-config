;;; al-lisp.el --- Additional functionality for all Lisp-like languages  -*- lexical-binding: t -*-

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


;;; Imenu sections

;; If you have sections in Lisp/Scheme files that begin with ";;;", you
;; may use the following code to add "Sections" entry in `imenu':
;;
;; (add-hook 'emacs-lisp-mode-hook 'al/lisp-imenu-add-sections)
;; (add-hook 'lisp-mode-hook 'al/lisp-imenu-add-sections)
;; (add-hook 'scheme-mode-hook 'al/lisp-imenu-add-sections)

(defvar al/lisp-imenu-sections-re "^;;;+ \\(.+\\)$"
  "Regexp for \"Sections\" imenu entries in Lisp modes.")

(defvar al/lisp-imenu-sections-group "Sections"
  "Group name in imenu index of \"Sections\" entries in Lisp modes.
If nil, put the entries in a top level.  See MENU-TITLE in
`imenu-generic-expression' variable for details.")

(declare-function al/add-to-imenu "al-imenu")

;;;###autoload
(defun al/lisp-imenu-add-sections (&optional regexp)
  "Add REGEXP as a \"Sections\" element to `imenu-generic-expression'.
If REGEXP is nil, use `al/lisp-imenu-sections-re'."
  (al/add-to-imenu (or regexp al/lisp-imenu-sections-re)
                   :title al/lisp-imenu-sections-group
                   :append t))

(provide 'al-lisp)

;;; al-lisp.el ends here
