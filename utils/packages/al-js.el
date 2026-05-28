;;; al-js.el --- Additional functionality for `js' package  -*- lexical-binding: t -*-

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

(require 'js)


;;; Imenu sections

;; To have "Sections" entry in javascript buffers:
;;
;; (add-hook 'js-mode-hook 'al/js-imenu-add-sections)

(defvar al/js-imenu-sections-re "^/// \\(.+\\)$"
  "Regexp for \"Sections\" imenu entries in `js-mode'.")

(declare-function al/lisp-imenu-add-sections "al-lisp")

;;;###autoload
(defun al/js-imenu-add-sections (&optional _regexp)
  "Add REGEXP as a \"Sections\" element to `imenu-generic-expression'.
If REGEXP is nil, use `al/imenu-sections-re'."
  (al/lisp-imenu-add-sections al/js-imenu-sections-re)
  (setq-local imenu-create-index-function #'al/js-imenu-create-index))

(declare-function imenu--generic-function "imenu" (patterns))

(defun al/js-imenu-create-index ()
  "Create an index alist for the current js buffer.
The function is suitable for `imenu-create-index-function' variable and
intended to be used instead of `js--imenu-create-index' in `js-mode'
buffers.  It adds the same entries as the latter function and also
create elements for `imenu-generic-expression'."
  (let ((js-index (js--imenu-create-index))
        (generic-index
         (save-excursion
           (save-restriction
             (widen)
             (imenu--generic-function imenu-generic-expression)))))
    (append js-index generic-index)))

(provide 'al-js)

;;; al-js.el ends here
