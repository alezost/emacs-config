;;; al-package-cmd.el --- Interactive commands related to Emacs package system

;; Copyright © 2013-2016 Alex Kost

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

(require 'cl-lib)
(require 'package)

(defvar al/package-archives
  '(("gnu"          . "http://elpa.gnu.org/packages/")
    ("marmalade"    . "https://marmalade-repo.org/packages/")
    ("melpa"        . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/"))
  "Alist of package archives used by `al/add-package-archive'.
This variable has the same form as `package-archives'.")

;;;###autoload
(defun al/add-package-archive (name)
  "Add archive to the value of `package-archives'.
NAME is an archive name from `al/package-archives'."
  (interactive
   (list (completing-read "Add package archive: "
                          (mapcar #'car al/package-archives))))
  (let ((archive (assoc name al/package-archives)))
    (when archive
      (add-to-list 'package-archives archive)
      (pp-eval-expression 'package-archives))))

;;;###autoload
(defun al/remove-package-archive (&optional name)
  "Remove archive to the value of `package-archives'.
NAME is an archive name from `package-archives'.
If NAME is nil (interactively, with \\[universal-argument]), \
remove all archives (i.e., set it to nil)."
  (interactive
   (list (unless current-prefix-arg
           (completing-read "Remove package archive: "
                            (mapcar #'car package-archives)))))
  (setq package-archives
        (and name
             (cl-remove-if (lambda (archive)
                             (equal name (car archive)))
                           package-archives)))
  (pp-eval-expression 'package-archives))

(provide 'al-package-cmd)

;;; al-package-cmd.el ends here
