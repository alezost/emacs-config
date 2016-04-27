;;; al-file.el --- Additional functionality for working with files

;; Copyright © 2012-2016 Alex Kost

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


;;; Backup

;; Setting `make-backup-file-name-function' is not enough as it is used
;; by `make-backup-file-name', but not by `find-backup-file-name', so
;; replace `make-backup-file-name-1' instead.

(defun al/make-backup-file-name-1 (file)
  "Return a new backup file path of a given FILE.
If the new path's directories do not exist, create them.
This function is intended to be used as a substitution for
`make-backup-file-name-1'."
  (let ((alist backup-directory-alist)
        (file (expand-file-name file))
	elt backup-directory abs-backup-directory backup-file)
    (while alist
      (setq elt (pop alist))
      (if (string-match (car elt) file)
	  (setq backup-directory (cdr elt)
		alist nil)))
    (if (null backup-directory)
        (setq backup-file file)
      (setq backup-file
            ;; New full path in backup dir tree.
            (concat (directory-file-name (expand-file-name backup-directory))
                    file)
            abs-backup-directory (file-name-directory backup-file))
      (if (and abs-backup-directory
               (not (file-exists-p abs-backup-directory)))
          (condition-case nil
              (make-directory abs-backup-directory 'parents)
            (file-error (setq backup-directory nil
                              abs-backup-directory nil)))))
    backup-file))

(defun al/backup-enable-predicate (name)
  "Function for `backup-enable-predicate'.
Do not backup su/sudo files."
  (and (normal-backup-enable-predicate name)
       (not (let ((method (file-remote-p name 'method)))
              (when (stringp method)
                (member method '("su" "sudo")))))))

(provide 'al-file)

;;; al-file.el ends here
