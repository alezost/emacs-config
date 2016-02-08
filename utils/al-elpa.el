;;; al-elpa.el --- Additional functionality for elpa and friends

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 9 Apr 2013

;;; Code:

(require 'cl-lib)

(defvar al/package-archives
  '(("gnu"          . "http://elpa.gnu.org/packages/")
    ("marmalade"    . "https://marmalade-repo.org/packages/")
    ("melpa"        . "http://melpa.org/packages/")
    ("melpa-stable" . "http://stable.melpa.org/packages/"))
  "Alist of package archives used by `al/add-package-archive'.
This variable has the same form as `package-archives'.")

(defvar package-archives)

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

(provide 'al-elpa)

;;; al-elpa.el ends here
