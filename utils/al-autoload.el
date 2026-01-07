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

(defun al/autoloads-file (directory)
  "Return the name of `autoloads' file for DIRECTORY."
  (let* ((dir  (expand-file-name directory))
         (base (file-name-nondirectory (directory-file-name dir)))
         ;; Package directories have "<name>[-<version>]" names.
         (name (and (string-match "\\`\\(.+?\\)[-.0-9]*\\'" base)
                    (match-string 1 base))))
    (expand-file-name (concat (or name base) "-autoloads.el") dir)))

(defun al/find-autoloads (directory)
  "Return a list of Emacs `autoloads' files in DIRECTORY."
  (car (directory-files
        directory 'full-name al/autoloads-regexp 'no-sort 1)))

(provide 'al-autoload)

;;; al-autoload.el ends here
