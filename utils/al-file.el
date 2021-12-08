;;; al-file.el --- Additional functionality for working with files

;; Copyright © 2016–2017, 2021 Alex Kost

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

(require 'cl-lib)

(defun al/file-if-exists (file)
  "Return FILE if it exists, or nil."
  (and (file-exists-p file) file))

(defun al/existing-files (&rest file-names)
  "Return a list of existing files from FILE-NAMES."
  (delq nil (mapcar #'al/file-if-exists file-names)))

(defun al/first-existing-file (&rest file-names)
  "Return the first existing file from FILE-NAMES."
  (cl-find-if #'file-exists-p file-names))

(defmacro al/setq-file (&rest body)
  "Like `setq' but for setting to file name values.
Check each file, and if it exists set the variable accordingly.
Example:

  (al/setq-file v1 \"/foo\"
                v2 \"/tmp\")

v2 will be set, while v1 will not."
  `(progn
     ,@(cl-loop for lst on body by #'cddr
                collect
                (let ((var  (car lst))
                      (file (cadr lst)))
                  `(let ((file ,file))
                     (when (file-exists-p file)
                       (setq ,var file)))))))

(defun al/file-regexp (&rest extensions)
  "Return regexp to match file name by EXTENSIONS."
  (rx-to-string `(and "." (or ,@extensions) string-end)
                'no-group))

(defun al/subdirs (directory)
  "Return list of DIRECTORY sub-directories."
  (cl-remove-if (lambda (file)
                  (or (string-match-p (rx "/." string-end) file)
                      (string-match-p (rx "/.." string-end) file)
                      (not (file-directory-p file))))
                (directory-files directory 'full-name nil 'no-sort)))

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

(defun al/append-files (in-files out-file &optional insert-file-names)
  "Insert the contents of IN-FILES into OUT-FILE.
IN-FILES is a list of file names.
OUT-FILE is a name of the output file.
If INSERT-FILE-NAMES is non-nil, insert a file name before the
contents of each file."
  (if (and (listp in-files)
           (> (length in-files) 1))
      (with-temp-buffer
        (dolist (file in-files)
          (when (file-regular-p file)
            (when insert-file-names
              (insert "\n" file "\n"))
            (insert-file-contents file)
            (goto-char (point-max))))
        (write-region nil nil out-file))
    (error "Give me more than 1 file to append")))

(defun al/check-file-name-length (fun file1 file2)
  ;; Evaluate: (file-newer-than-file-p (make-string 257 ?a) "")
  ;; and it gives the following error:
  ;;
  ;;   Getting attributes: File name too long
  ;;
  ;; This is problematic because `after-find-file' calls
  ;; `file-newer-than-file-p' to check auto-save file, which may have a
  ;; VERY LONG name.
  "Call FUN if FILE1 and FILE2 are not too long.
This function is intendend to be used as an 'around' advice for
`file-newer-than-file-p'."
  (and (< 257 (length file1))
       (< 257 (length file2))
       (funcall fun file1 file2)))

(provide 'al-file)

;;; al-file.el ends here
