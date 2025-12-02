;;; al-package-cmd.el --- Interactive commands related to Emacs package system  -*- lexical-binding: t -*-

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

(require 'seq)
(require 'package)
(require 'transient)
(require 'al-general)
(require 'al-quelpa)

(defvar al/package-archives
  '(("elpa gnu"     . "https://elpa.gnu.org/packages/")
    ("elpa nongnu"  . "https://elpa.nongnu.org/nongnu/")
    ("melpa"        . "https://melpa.org/packages/")
    ("melpa-stable" . "https://stable.melpa.org/packages/"))
  "Alist of package archives used by `al/add-package-archive'.
This variable has the same form as `package-archives'.")

;;;###autoload
(defun al/add-package-archive (name)
  "Add archive to the value of `package-archives'.
NAME is an archive name from `al/package-archives'."
  (interactive
   (list (completing-read "Add package archive: "
                          (mapcar #'car al/package-archives))))
  (when-let* ((archive (assoc name al/package-archives)))
    (al/pushnew package-archives archive)))

;;;###autoload
(defun al/remove-package-archive (&optional name)
  "Remove archive from the value of `package-archives'.
NAME is an archive name from `package-archives'.
If NAME is nil (interactively, with \\[universal-argument]),
remove all archives (i.e., set it to nil)."
  (interactive
   (list (unless current-prefix-arg
           (completing-read "Remove package archive: "
                            (mapcar #'car package-archives)))))
  (setq package-archives
        (and name
             (seq-remove (lambda (archive)
                           (equal name (car archive)))
                         package-archives))))


;;; Transient interface for archives and packages

(transient-define-argument al/package-ui:main-packages ()
  :description (concat "recipes from "
                       (propertize (symbol-name 'al/main-packages)
                                   'face 'font-lock-constant-face))
  :class 'transient-switch
  :key "-m"
  :argument "main")

(transient-define-argument al/package-ui:extra-packages ()
  :description (concat "recipes from "
                       (propertize (symbol-name 'al/extra-packages)
                                   'face 'font-lock-constant-face))
  :class 'transient-switch
  :key "-e"
  :argument "extra")

(defun al/package-ui-archives-info ()
  "Return a fontified string with `package-archives' value."
  (concat
   (propertize "package-archives" 'face 'font-lock-constant-face)
   " value:"
   (if (null package-archives)
       " nil"
     (with-temp-buffer
       (emacs-lisp-mode)
       (insert "\n")
       (pp package-archives (current-buffer))
       (font-lock-ensure)
       (buffer-substring (point-min) (point-max))))))

(transient-define-suffix al/package-ui:add-archive ()
  (interactive)
  (call-interactively #'al/add-package-archive)
  (al/package-ui))

(transient-define-suffix al/package-ui:add-all-archives ()
  (interactive)
  (setq package-archives al/package-archives)
  (al/package-ui))

(transient-define-suffix al/package-ui:remove-archive ()
  (interactive)
  (call-interactively #'al/remove-package-archive)
  (al/package-ui))

(transient-define-suffix al/package-ui:remove-all-archives ()
  (interactive)
  (setq package-archives nil)
  (al/package-ui))

(transient-define-suffix al/package-ui:install-from-recipes (&rest recipes)
  "Call `al/quelpa' with RECIPES."
  (interactive
   (let ((args (transient-args 'al/package-ui)))
     (append (and (transient-arg-value "main" args)
                  al/main-packages)
             (and (transient-arg-value "extra" args)
                  al/extra-packages))))
  (if recipes
      (apply #'al/quelpa recipes)
    (message "Choose \"main\" and/or \"extra\" recipes.")
    (al/package-ui)))

(declare-function al/switch-to-packages "al-buffer.el" nil)

;;;###autoload (autoload 'al/package-ui "al-package-cmd" nil t)
(transient-define-prefix al/package-ui ()
  "Interface for Emacs packages, recipes, archives, etc."
  :value '("main")
  ["Package archives"
   (:info #'al/package-ui-archives-info :format "%d")
   (:info "")
   ("au" "update archive contents (to refresh package list)"
    package-refresh-contents :transient t)]
  [[("aa" "add archive"         al/package-ui:add-archive)
    ("ar" "remove archive"      al/package-ui:remove-archive)]
   [("aA" "add all archives"    al/package-ui:add-all-archives)
    ("aR" "remove all archives" al/package-ui:remove-all-archives)]
   [("l" "package list" al/switch-to-packages)]]
  ["Install/upgrade package(s)"
   [(:info
     (concat (propertize " using " 'face 'transient-heading)
             (propertize "quelpa" 'face 'font-lock-constant-face)
             ":")
     :format "%d")
    ("iq" "package from melpa recipe" quelpa)
    ("im" "package from my recipe" al/quelpa)
    ""
    (al/package-ui:main-packages)
    (al/package-ui:extra-packages)
    ("iA" "packages from my recipes" al/package-ui:install-from-recipes)]
   [(:info
     (concat (propertize " using " 'face 'transient-heading)
             (propertize "package-install" 'face 'font-lock-constant-face)
             ":")
     :format "%d")
    ("ia" "package from archives" package-install)
    "" ""
    ("R" "remove package" package-delete)]])

(provide 'al-package-cmd)

;;; al-package-cmd.el ends here
