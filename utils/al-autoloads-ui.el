;;; al-autoloads-ui.el --- User interface to generate autoloads of Emacs packages  -*- lexical-binding: t -*-

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

(require 'transient)
(require 'al-autoload)
(require 'al-misc)

;; Defined in "init.el"
(defvar al/emacs-utils-dir)
(defvar al/emacs-my-packages-dir)
(defvar al/emacs-utils-autoloads)
(defvar al/emacs-my-package-autoloads)
(defvar al/emacs-elpa-package-autoloads)

(defun al/autoloads-ui-read-directory (prompt initial-input history)
  "Read directory for `al/autoloads-ui'."
  (completing-read prompt
                   (list al/emacs-utils-dir
                         al/emacs-my-packages-dir
                         package-user-dir)
                   nil nil initial-input history))

(defun al/autoloads-ui-read-output-file (prompt initial-input history)
  "Read output autoloads file for `al/autoloads-ui'."
  (completing-read prompt
                   (list al/emacs-utils-autoloads
                         al/emacs-my-package-autoloads
                         al/emacs-elpa-package-autoloads)
                   nil nil initial-input history))


(transient-define-argument al/autoloads-ui:directory ()
  :description "Directory with elisp files"
  :class 'transient-option
  :key "d"
  :argument "directory="
  :always-read t
  :reader #'al/autoloads-ui-read-directory)

(transient-define-argument al/autoloads-ui:output-file ()
  :description "Output file with autoloads"
  :class 'transient-option
  :key "o"
  :argument "output-file="
  :reader #'al/autoloads-ui-read-output-file)

(transient-define-argument al/autoloads-ui:add-to-path ()
  :description "Add dir(s) with autoloads to load-path or not"
  :class 'transient-option
  :key "l"
  :argument "add-to-path="
  :choices '(append prepend))

(transient-define-argument al/autoloads-ui:subdirs ()
  :description "Use only directory [nil], only its sub-directories [only], or both [t]"
  :class 'transient-option
  :key "s"
  :argument "subdirs="
  :choices '(only t))

;;;###autoload (autoload 'al/autoloads-ui "al-autoloads-ui" nil t)
(transient-define-prefix al/autoloads-ui ()
  "Generate \"autoloads.el\" files."
  :value (list (concat "directory=" al/emacs-utils-dir)
               (concat "output-file=" al/emacs-utils-autoloads))
  ["Arguments"
   (al/autoloads-ui:directory)
   (al/autoloads-ui:output-file)
   (al/autoloads-ui:add-to-path)
   (al/autoloads-ui:subdirs)]
  ["Generate autoloads"
   [("g" "generate" al/autoloads-ui-generate)]])

(transient-define-suffix al/autoloads-ui-generate
  (directory output-file add-to-path subdirs)
  "Call `al/generate-autoloads' with the specified arguments."
  (interactive
   (let ((args (transient-args 'al/autoloads-ui)))
     (list (transient-arg-value "directory=" args)
           (transient-arg-value "output-file=" args)
           (al/intern (transient-arg-value "add-to-path=" args))
           (al/intern (transient-arg-value "subdirs=" args)))))
  (al/generate-autoloads directory
                         :output-file output-file
                         :add-to-path add-to-path
                         :subdirs subdirs))

(provide 'al-autoloads-ui)

;;; al-autoloads-ui.el ends here
