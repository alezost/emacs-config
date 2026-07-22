;;; init.el --- File symlinked by `user-init-file'  -*- lexical-binding: t -*-

;; Copyright © 2012–2026 Alex Kost

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

(load (expand-file-name "../utils/general/al-places"
                        (file-name-directory
                         (file-truename load-file-name))))

(require 'al-places nil t)      ; to silence byte-compiler


;;; (Auto)loading various files

(defvar al/emacs-utils-autoloads (al/emacs-utils-dir-file "utils-autoloads.el")
  "`autoloads' file for my utils.")
(defvar al/emacs-my-packages-autoloads (al/emacs-data-dir-file "my-autoloads.el")
  "`autoloads' file for my packages.")
(defvar al/emacs-elpa-packages-autoloads (al/emacs-data-dir-file "elpa-autoloads.el")
  "`autoloads' file for ELPA packages.")

(setq
 load-prefer-newer t
 package-user-dir (al/emacs-data-dir-file "elpa")
 custom-file (al/emacs-init-dir-file "custom.el"))

(defvar al/initial-load-path load-path)
(push al/emacs-general-utils-dir load-path)
(push (al/emacs-utils-dir-file "packages") load-path)
(push al/emacs-my-packages-dir load-path)

(require 'al-general)

(al/title-message "Loading init files")
(defun al/init-load (file)
  "Load FILE from `al/emacs-init-dir'."
  (al/load (al/emacs-init-dir-file file)))

(defvar al/init-files
  '("keys"
    "text"
    "packages"
    "settings"
    "files"
    "prog"
    "time"
    "file-modes"
    "mmedia"
    "net"
    "visual"
    "games"
    "custom"))

(dolist (file al/init-files)
  (condition-case error
      (al/init-load file)
    (error (message (concat "ERROR during loading \""
                            file "\" init file: %S")
                    error)
           nil)))

(defvar al/load-paths nil)

(declare-function al/generate-autoloads "al-autoload")

(defun al/load-autoloads (name directory autoloads-file &rest args)
  "Load AUTOLOADS-FILE, generate it for DIRECTORY if needed.

NAME is a string used for messages.

Additional ARGS are sent to `al/generate-autoloads'.

Do not alter `load-path'.  Instead, push added `load-path' to
`al/load-paths' for further use."
  (declare (indent 1))
  (when (file-exists-p directory)
    (al/title-message (concat "Autoloading " name))
    (unless (file-exists-p autoloads-file)
      (condition-case error
          (progn
            (require 'al-autoload)
            (apply #'al/generate-autoloads directory
                   :output-file autoloads-file
                   args))
        (error (message (concat "ERROR during generating "
                                name " autoloads: %S")
                        error))))
    (condition-case error
        (let ((count (length load-path)))
          (al/load autoloads-file)
          ;; Pick the freshly added paths for further use.
          (push (seq-subseq load-path 0 (- count))
                al/load-paths))
      (error (message (concat "ERROR during loading "
                              name " autoloads: %S")
                      error)))))

(defvar al/autoloads-presets
  `(("utils"
     ,al/emacs-utils-dir
     ,al/emacs-utils-autoloads
     :add-to-path prepend
     :subdirs only)
    ("my packages"
     ,al/emacs-my-packages-dir
     ,al/emacs-my-packages-autoloads
     :add-to-path prepend
     :subdirs t)
    ("ELPA packages"
     ,package-user-dir
     ,al/emacs-elpa-packages-autoloads
     :add-to-path prepend
     :subdirs only))
  "Presets for \"autoloads.el\" files.")

(pcase-dolist (`(,name ,dir ,file . ,args)
               al/autoloads-presets)
  (apply #'al/load-autoloads name dir file args))

;; Prepend paths added by the above autoloads to `load-path' in reverse
;; order.  So the first loaded autoloads have precedence over the last
;; ones.
(setq load-path
      (nconc (apply #'nconc (nreverse al/load-paths))
             al/initial-load-path))


;;; Final settings

;; Settings that cannot be set in other config files because they are
;; loaded before external packages are autoloaded.

(al/title-message "Final settings")

;; These hooks cannot be set in my init files because my utils, my
;; packages, and external packages are autoloaded after loading the init
;; files.
(al/add-hook-maybe 'after-save-hook 'al/check-parens)
;; (al/add-hook-maybe 'after-change-major-mode-hook
;;   'al/set-default-input-method)
(al/add-hook-maybe 'window-configuration-change-hook
  'al/set-windows-num-property)
(al/add-hook-maybe 'text-mode-hook
  '(visual-line-mode
    hl-line-mode
    abbrev-mode
    al/no-syntactic-font-lock
    al/show-trailing-whitespace))
(al/add-hook-maybe 'prog-mode-hook
  '(hl-line-mode
    hl-todo-mode
    abbrev-mode
    al/set-comment-column
    al/show-trailing-whitespace))
(al/add-hook-maybe 'messages-buffer-mode-hook
  (list 'hl-todo-mode
        (lambda () (setq buffer-read-only nil))))
(al/call-after-init 'which-key-mode)
(al/call-after-frame-kill 'al/save-everything)
(al/add-hook-maybe 'kill-emacs-hook 'al/save-everything)

(al/eval-after-load al-process
  :load t
  (advice-add 'insert-directory :around #'al/call-with-locale)
  (al/enable-process-hooks))

(al/eval-after-load al-server
  :load t
  (when-let* ((name (al/server-name)))
    (setq al/server-running? t)
    (when (equal name "emms")
      (al/call-after-init
       '(al/save-place-mode
         al/recentf-mode
         appt-activate)))))

(message "Garbage collected %d times." gcs-done)
(al/title-message "Emacs config has been loaded")

;;; init.el ends here
