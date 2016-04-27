;;; al-file.el --- Additional functionality for working with files

;; Copyright © 2016 Alex Kost

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

(defun al/add-to-auto-mode-alist (specs)
  "Add SPECS to `auto-mode-alist'.
Each specification from SPECS list may have one of the following forms:

  (MODE-NAME . REGEXPS)
  (MODE-NAME REGEXP-OR-LIST t)

REGEXP-OR-LIST is either a regexp (string), or a list of regexps.
For the first form, specifications are added at the beginning of
`auto-mode-alist'; for the second form it is added at the end."
  (cl-flet ((add (mode regexp &optional append?)
              (add-to-list 'auto-mode-alist (cons regexp mode) append?)))
    (dolist (spec specs)
      (pcase spec
        (`(,mode ,str-or-lst t)
         (if (stringp str-or-lst)
             (add mode str-or-lst t)
           (dolist (regexp str-or-lst)
             (add mode regexp t))))
        (`(,mode . ,regexps)
         (dolist (regexp regexps)
           (add mode regexp)))))))

(provide 'al-file)

;;; al-file.el ends here
