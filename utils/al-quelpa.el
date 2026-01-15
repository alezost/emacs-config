;;; al-quelpa.el --- Additional functionality for Quelpa  -*- lexical-binding: t -*-

;; Copyright © 2014–2025 Alex Kost

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

(require 'seq)
(require 'quelpa nil t)  ; `quelpa' does not exist in bootstrap case
(require 'al-file)

(defvar al/main-packages nil
  "List of main packages that should be installed in a common way.")

(defvar al/extra-packages nil
  "List of packages used from rarely to never.")

(defun al/all-packages ()
  "Return all package recipes I use."
  (append al/main-packages
          al/extra-packages))

(defun al/package-name (name-or-recipe)
  "Return package name (symbol) by NAME-OR-RECIPE."
  (if (listp name-or-recipe)
      (car name-or-recipe)
    name-or-recipe))

(defun al/package-recipe (name-or-recipe)
  "Return package recipe by NAME-OR-RECIPE."
  (if (listp name-or-recipe)
      name-or-recipe
    (seq-find (lambda (recipe)
                (eq name-or-recipe (al/package-name recipe)))
              (al/all-packages))))

(defun al/read-package-name ()
  "Prompt for and return a package name (symbol)."
  (let ((names (mapcar (lambda (recipe)
                         (symbol-name (al/package-name recipe)))
                       (al/all-packages))))
    (intern (completing-read "Update/install: " names nil t))))

(defvar al/emacs-my-packages-dir)   ; defined in "init.el"

;;;###autoload
(defun al/quelpa (&rest recipes)
  "Install/update packages using RECIPES.

Each recipe from RECIPES should be either a MELPA package
name (symbol) or a full recipe (list).

Interactively, prompt for a package to update/install.

With \\[universal-argument], update all packages except `al/extra-packages'.
With \\[universal-argument] \\[universal-argument], update all packages."
  (interactive
   (cond ((equal current-prefix-arg '(4))
          al/main-packages)
         ((equal current-prefix-arg '(16))
          (al/all-packages))
         (t (list (al/package-recipe (al/read-package-name))))))
  (unless (fboundp 'quelpa)
    ;; Bootstrap `quelpa' if it doesn't exist.
    (with-temp-buffer
      (url-insert-file-contents
       "https://raw.githubusercontent.com/quelpa/quelpa/master/quelpa.el")
      (eval-buffer)))
  (let ((my-packages (and (file-exists-p al/emacs-my-packages-dir)
                          (mapcar #'intern
                                  (al/subdirs al/emacs-my-packages-dir t)))))
    (dolist (recipe recipes)
      (let ((pkg-name (al/package-name recipe)))
        (if (memq pkg-name my-packages)
            (message "Ignoring my package `%S'." pkg-name)
          (quelpa recipe))))))

(provide 'al-quelpa)

;;; al-quelpa.el ends here
