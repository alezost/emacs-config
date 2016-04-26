;;; al-imenu.el --- Additional functionality for imenu

;; Copyright © 2014-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;;; Lisp sections

;; If you have sections in lisp/elisp files that begin with ";;;", you
;; may use the following code to add "Sections" entry in `imenu':
;;
;; (add-hook 'emacs-lisp-mode-hook 'al/imenu-add-sections)
;; (add-hook 'lisp-mode-hook 'al/imenu-add-sections)

(defvar al/imenu-sections-re "^;;; \\(.+\\)$"
  "Regexp used for \"Sections\" imenu entries.")

(defvar al/imenu-sections-group "Sections"
  "Group name in imenu index of \"Sections\" entries.
If nil, put the entries in a top level.  See MENU-TITLE in
`imenu-generic-expression' variable for details.")

;;;###autoload
(defun al/imenu-add-sections (&optional regexp)
  "Add REGEXP as a \"Sections\" element to `imenu-generic-expression'.
If REGEXP is nil, use `al/imenu-sections-re'."
  (add-to-list
   'imenu-generic-expression
   (list al/imenu-sections-group (or regexp al/imenu-sections-re) 1)
   t))


;;; JS sections

;; To have "Sections" entry in javascript buffers:
;;
;; (add-hook 'js-mode-hook 'al/imenu-add-js-sections)

(defvar al/imenu-js-sections-re "^/// \\(.+\\)$"
  "Regexp used for \"Sections\" imenu entries in `js-mode'.")

;;;###autoload
(defun al/imenu-add-js-sections (&optional regexp)
  "Add REGEXP as a \"Sections\" element to `imenu-generic-expression'.
If REGEXP is nil, use `al/imenu-sections-re'."
  (al/imenu-add-sections al/imenu-js-sections-re)
  (setq-local imenu-create-index-function 'al/js-imenu-create-index))

(declare-function js--imenu-create-index "js" nil)
(declare-function imenu--generic-function "imenu" (patterns))

(defun al/js-imenu-create-index ()
  "Create an index alist for the current js buffer.
The function is suitable for `imenu-create-index-function'
variable and intended to be used instead of
`js--imenu-create-index' in js buffers.  It adds the same entries
as the latter function and also create elements for
`imenu-generic-expression'."
  (let ((js-index (js--imenu-create-index))
        (generic-index
         (save-excursion
           (save-restriction
             (widen)
             (imenu--generic-function imenu-generic-expression)))))
    (append js-index generic-index)))


;;; `use-package' entries

;; Idea from <https://github.com/jwiegley/use-package/issues/80>.

(defvar al/imenu-use-package-re
  (rx bol "(use-package" (+ whitespace)
      (? ?\")
      (group (+ (or (syntax word) (syntax symbol))))
      (? ?\"))
  "Regexp used for `use-package' entries in imenu.")

(defvar al/imenu-use-package-group "use-package"
  "Group name in imenu index of use-package entries.
If nil, put the entries in a top level.  See MENU-TITLE in
`imenu-generic-expression' variable for details.")

;;;###autoload
(defun al/imenu-add-use-package ()
  "Add `al/imenu-use-package-re' to `imenu-generic-expression'."
  (add-to-list
   'imenu-generic-expression
   (list al/imenu-use-package-group al/imenu-use-package-re 1)))


;;; (with-)eval-after-load entries

(defvar al/imenu-eval-after-load-re
  (rx bol "(" (zero-or-one "with-") "eval-after-load" (+ whitespace)
      (zero-or-one (or ?\" ?'))
      (group (+ (or (syntax word) (syntax symbol))))
      (zero-or-one ?\"))
  "Regexp used for `eval-after-load' and `with-eval-after-load'
entries in imenu.")

(defvar al/imenu-eval-after-load-group "(with-)eval-after-load")

;;;###autoload
(defun al/imenu-add-eval-after-load ()
  "Add `al/imenu-eval-after-load-re' to `imenu-generic-expression'."
  (add-to-list
   'imenu-generic-expression
   (list al/imenu-eval-after-load-group
         al/imenu-eval-after-load-re
         1)))

(provide 'al-imenu)

;;; al-imenu.el ends here
