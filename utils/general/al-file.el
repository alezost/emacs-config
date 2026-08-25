;;; al-file.el --- Additional functionality for working with files  -*- lexical-binding: t -*-

;; Copyright © 2016–2026 Alex Kost

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

(eval-when-compile
  (require 'cl-lib))
(require 'seq)
(require 'count)
(require 'al-list)

(defun al/file-if-exists (file)
  "Return FILE if it exists, or nil."
  (and (file-exists-p file) file))

(defun al/existing-files (&rest file-names)
  "Return a list of existing files from FILE-NAMES."
  (seq-filter #'file-exists-p file-names))

(defun al/subdirs (directory &optional base)
  "Return list of DIRECTORY sub-directories.
If BASE is non-nil, return only directories names, otherwise return full
absolute file names."
  (let ((subdirs
         (seq-filter #'file-directory-p
                     (directory-files directory 'full "\\`[^.]" 'no-sort))))
    (if (and subdirs base)
        (mapcar (lambda (dir)
                  (file-name-nondirectory
                   (directory-file-name dir)))
                subdirs)
      subdirs)))

(defun al/append-files (in-files out-file &optional insert-file-names)
  "Insert the contents of IN-FILES into OUT-FILE.
IN-FILES is a list of file names.
OUT-FILE is a name of the output file.
If INSERT-FILE-NAMES is non-nil, insert a file name before the
contents of each file."
  (if (and (listp in-files)
           (>1 in-files))
      (with-temp-buffer
        (dolist (file in-files)
          (when (file-regular-p file)
            (when insert-file-names
              (insert "\n" file "\n"))
            (insert-file-contents file)
            (goto-char (point-max))))
        (write-region nil nil out-file))
    (error "Give me more than 1 file to append")))

(provide 'al-file)

;;; al-file.el ends here
