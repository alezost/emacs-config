;;; al-package-tui.el --- Transient interface for Emacs package system  -*- lexical-binding: t -*-

;; Copyright © 2025–2026 Alex Kost

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

(require 'package)
(require 'transient)
(require 'al-package-cmd)
(require 'al-quelpa)
(require 'al-color)

(transient-define-argument al/package-tui:main-packages ()
  :description (concat "recipes from "
                       (al/with-face 'font-lock-constant-face
                         (symbol-name 'al/main-packages)))
  :class 'transient-switch
  :key "-m"
  :argument "main")

(transient-define-argument al/package-tui:extra-packages ()
  :description (concat "recipes from "
                       (al/with-face 'font-lock-constant-face
                         (symbol-name 'al/extra-packages)))
  :class 'transient-switch
  :key "-e"
  :argument "extra")

(defun al/package-tui-archives-info ()
  "Return a fontified string with `package-archives' value."
  (concat
   (al/with-face 'font-lock-constant-face "package-archives")
   " value:"
   (if (null package-archives)
       " nil"
     (with-temp-buffer
       (emacs-lisp-mode)
       (insert "\n")
       (pp package-archives (current-buffer))
       (font-lock-ensure)
       (buffer-substring (point-min) (point-max))))))

(transient-define-suffix al/package-tui:add-archive ()
  (interactive)
  (call-interactively #'al/add-package-archive)
  (al/package-tui))

(transient-define-suffix al/package-tui:add-all-archives ()
  (interactive)
  (setq package-archives al/package-archives)
  (al/package-tui))

(transient-define-suffix al/package-tui:remove-archive ()
  (interactive)
  (call-interactively #'al/remove-package-archive)
  (al/package-tui))

(transient-define-suffix al/package-tui:remove-all-archives ()
  (interactive)
  (setq package-archives nil)
  (al/package-tui))

(transient-define-suffix al/package-tui:install-from-recipes (&rest recipes)
  "Call `al/quelpa' with RECIPES."
  (interactive
   (let ((args (transient-args 'al/package-tui)))
     (append (and (transient-arg-value "main" args)
                  al/main-packages)
             (and (transient-arg-value "extra" args)
                  al/extra-packages))))
  (if recipes
      (apply #'al/quelpa recipes)
    (message "Choose \"main\" and/or \"extra\" recipes.")
    (al/package-tui)))

(declare-function al/switch-to-packages "al-buffer.el" nil)

;;;###autoload (autoload 'al/package-tui "al-package-tui" nil t)
(transient-define-prefix al/package-tui ()
  "Interface for Emacs packages, recipes, archives, etc."
  :value '("main")
  ["Package archives"
   (:info #'al/package-tui-archives-info :format "%d")
   (:info "")
   ("au" "update archive contents (to refresh package list)"
    package-refresh-contents :transient t)]
  [[("aa" "add archive"         al/package-tui:add-archive)
    ("ar" "remove archive"      al/package-tui:remove-archive)]
   [("aA" "add all archives"    al/package-tui:add-all-archives)
    ("aR" "remove all archives" al/package-tui:remove-all-archives)]
   [("l" "package list" al/switch-to-packages)]]
  ["Install/upgrade package(s)"
   [(:info
     (concat (al/with-face 'transient-heading " using ")
             (al/with-face 'font-lock-constant-face "quelpa")
             ":")
     :format "%d")
    ("iq" "package from melpa recipe" quelpa)
    ("im" "package from my recipe" al/quelpa)
    ""
    (al/package-tui:main-packages)
    (al/package-tui:extra-packages)
    ("iA" "packages from my recipes" al/package-tui:install-from-recipes)]
   [(:info
     (concat (al/with-face 'transient-heading " using ")
             (al/with-face 'font-lock-constant-face "package-install")
             ":")
     :format "%d")
    ("ia" "package from archives" package-install)
    "" ""
    ("R" "remove package" package-delete)]])

(provide 'al-package-tui)

;;; al-package-tui.el ends here
