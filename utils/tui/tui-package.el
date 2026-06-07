;;; tui-package.el --- Transient interface for Emacs package system  -*- lexical-binding: t -*-

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
(require 'al-visual)

(transient-define-argument tui/package:main-packages ()
  :description (concat "recipes from "
                       (al/with-face 'font-lock-constant-face
                         (symbol-name 'al/main-packages)))
  :class 'transient-switch
  :key "-m"
  :argument "main")

(transient-define-argument tui/package:extra-packages ()
  :description (concat "recipes from "
                       (al/with-face 'font-lock-constant-face
                         (symbol-name 'al/extra-packages)))
  :class 'transient-switch
  :key "-e"
  :argument "extra")

(defun tui/package-archives-info ()
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

(transient-define-suffix tui/package:add-archive ()
  (interactive)
  (call-interactively #'al/add-package-archive)
  (tui/package))

(transient-define-suffix tui/package:add-all-archives ()
  (interactive)
  (setq package-archives al/package-archives)
  (tui/package))

(transient-define-suffix tui/package:remove-archive ()
  (interactive)
  (call-interactively #'al/remove-package-archive)
  (tui/package))

(transient-define-suffix tui/package:remove-all-archives ()
  (interactive)
  (setq package-archives nil)
  (tui/package))

(transient-define-suffix tui/package:install-from-recipes (&rest recipes)
  "Call `al/quelpa' with RECIPES."
  (interactive
   (let ((args (transient-args 'tui/package)))
     (append (and (transient-arg-value "main" args)
                  al/main-packages)
             (and (transient-arg-value "extra" args)
                  al/extra-packages))))
  (if recipes
      (apply #'al/quelpa recipes)
    (message "Choose \"main\" and/or \"extra\" recipes.")
    (tui/package)))

(declare-function al/switch-to-packages "al-buffer.el" nil)

;;;###autoload (autoload 'tui/package "tui-package" nil t)
(transient-define-prefix tui/package ()
  "Interface for Emacs packages, recipes, archives, etc."
  :value '("main")
  ["Package archives"
   (:info #'tui/package-archives-info :format "%d")
   (:info "")
   ("au" "update archive contents (to refresh package list)"
    package-refresh-contents :transient t)]
  [[("aa" "add archive"         tui/package:add-archive)
    ("ar" "remove archive"      tui/package:remove-archive)]
   [("aA" "add all archives"    tui/package:add-all-archives)
    ("aR" "remove all archives" tui/package:remove-all-archives)]
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
    (tui/package:main-packages)
    (tui/package:extra-packages)
    ("iA" "packages from my recipes" tui/package:install-from-recipes)]
   [(:info
     (concat (al/with-face 'transient-heading " using ")
             (al/with-face 'font-lock-constant-face "package-install")
             ":")
     :format "%d")
    ("ia" "package from archives" package-install)
    "" ""
    ("R" "remove package" package-delete)]])

(provide 'tui-package)

;;; tui-package.el ends here
