;;; al-autoload.el --- Additional functionality to autoload Emacs packages  -*- lexical-binding: t -*-

;; Copyright © 2013–2025 Alex Kost

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

(defvar al/autoloads-regexp
  (rx "-autoloads.el" string-end)
  "Regexp to match Emacs `autoloads' file.")

(defmacro al/autoload (file &rest symbols)
  "Autoload (unquoted) SYMBOLS from file as interactive commands."
  (declare (indent 1))
  `(progn
     ,@(mapcar (lambda (symbol)
                 `(autoload ',symbol ,file nil t))
               symbols)))

(defun al/autoloads-file (directory)
  "Return the name of `autoloads' file for DIRECTORY."
  (let* ((dir  (expand-file-name directory))
         (base (file-name-nondirectory (directory-file-name dir))))
    (expand-file-name (concat base "-autoloads.el") dir)))

(defun al/find-autoloads (directory)
  "Return a list of Emacs `autoloads' files in DIRECTORY."
  (car (directory-files
        directory 'full-name al/autoloads-regexp 'no-sort 1)))

(defun al/update-autoloads (&rest dirs)
  "Update the contents of `autoloads' files for all DIRS."
  (require 'loaddefs-gen)
  (dolist (dir dirs)
    (loaddefs-generate dir (al/autoloads-file dir))))

(provide 'al-autoload)

;;; al-autoload.el ends here
