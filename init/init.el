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


;;; Location of various files

(defmacro al/file-accessors (name val)
  "Define variable and function for accessing my directories."
  (let ((dir-var  (intern (concat "al/" name "-dir")))
        (file-fun (intern (concat "al/" name "-dir-file"))))
    `(progn
       (defvar ,dir-var ,val)
       (defun ,file-fun (file)
         ,(format "Return full file name of a FILE placed in `%s'."
                  dir-var)
         (expand-file-name file ,dir-var)))))

(al/file-accessors "emacs-init"
                    (file-name-directory
                     (file-truename load-file-name)))
(al/file-accessors "emacs"         (al/emacs-init-dir-file "../"))
(al/file-accessors "emacs-data"    (al/emacs-dir-file "data"))
(al/file-accessors "emacs-utils"   (al/emacs-dir-file "utils"))
(al/file-accessors "emacs-my-packages" (al/emacs-dir-file "packages"))
(al/file-accessors "gnus"          (al/emacs-data-dir-file "gnus"))
(al/file-accessors "gnus-news"     (al/gnus-dir-file "news"))
(al/file-accessors "gnus-mail"     (al/gnus-dir-file "mail"))
(al/file-accessors "gnus-saved"    (al/gnus-dir-file "saved"))

(al/file-accessors "config"        "~/config")
(al/file-accessors "notes"         "~/notes")
(al/file-accessors "progs"         "~/progs")
(al/file-accessors "journal"       (al/notes-dir-file  "journal"))
(al/file-accessors "music"         "~/music")
(al/file-accessors "sound"         "~/docs/audio/small")
(al/file-accessors "tmp"           "~/tmp")
(al/file-accessors "src"           "~/src")
(al/file-accessors "devel"         "~/devel")
(al/file-accessors "download"      "~/downloads")
(al/file-accessors "math"          "~/maths")


;;; Guix stuff

(al/file-accessors "guix-profile" "~/.guix-profiles")
(al/file-accessors "guix-system-profile" "/run/current-system/profile")

(defvar al/guix-system?
  (file-exists-p al/guix-system-profile-dir)
  "Non-nil, if current OS is GuixSD.")

(defvar al/guix-profile-names
  '("emacs" "fonts" "games" "build" "guile" "misc" "main"))

(defun al/guix-profile (name)
  "Return file name of my guix profile with NAME."
  (al/guix-profile-dir-file (concat name "/" name)))

(defun al/guix-profiles ()
  "Return a list of all my guix profiles."
  (mapcar #'al/guix-profile al/guix-profile-names))

(al/file-accessors "guix-user-profile" (al/guix-profile "main"))


;;; (Auto)loading various files

(defvar al/pure-config? (getenv "EMPURE")
  "Non-nil, if external packages should not be loaded.")

(defvar al/emacs-utils-autoloads (al/emacs-utils-dir-file "utils-autoloads.el")
  "`autoloads' file for my utils.")
(defvar al/emacs-my-package-autoloads (al/emacs-data-dir-file "my-autoloads.el")
  "`autoloads' file for my packages.")
(defvar al/emacs-elpa-package-autoloads (al/emacs-data-dir-file "elpa-autoloads.el")
  "`autoloads' file for ELPA packages.")

(setq
 load-prefer-newer t
 package-user-dir (al/emacs-data-dir-file "elpa")
 custom-file (al/emacs-init-dir-file "custom.el"))

(push al/emacs-utils-dir load-path)

(require 'al-general)
(al/title-message "Loading necessary utils")
(require 'al-file)
(require 'al-text)
(require 'al-key)

(al/title-message "Loading init files")
(defun al/init-load (file)
  "Load FILE from `al/emacs-init-dir'."
  (al/load (al/emacs-init-dir-file file)))
(mapc #'al/init-load
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
        "dict"
        "visual"
        "games"
        "custom"))

(al/title-message "Autoloading utils")
(unless (file-exists-p al/emacs-utils-autoloads)
  (with-demoted-errors "ERROR during generating utils autoloads: %S"
    (require 'al-autoload)
    (al/generate-autoloads al/emacs-utils-dir
                           :output-file al/emacs-utils-autoloads)))
(al/load al/emacs-utils-autoloads)

;; Autoloading external packages.
(unless al/pure-config?
  (when (file-exists-p package-user-dir)
    (al/title-message "Autoloading ELPA packages")
    (unless (file-exists-p al/emacs-elpa-package-autoloads)
      (with-demoted-errors "ERROR during generating ELPA packages autoloads: %S"
        (require 'al-autoload)
        (al/generate-autoloads package-user-dir
                               :output-file al/emacs-elpa-package-autoloads
                               :add-to-path 'prepend
                               :subdirs 'only)))
    (al/load al/emacs-elpa-package-autoloads))

  (when (file-exists-p al/guix-profile-dir)
    (al/title-message "Autoloading Guix packages")
    (with-demoted-errors "ERROR during autoloading Guix packages: %S"
      (when (require 'al-guix-autoload nil t)
        (apply #'al/guix-autoload-emacs-packages
               (al/guix-profiles)))))

  (when (file-exists-p al/emacs-my-packages-dir)
    (al/title-message "Autoloading my packages")
    (unless (file-exists-p al/emacs-my-package-autoloads)
      (with-demoted-errors "ERROR during generating my packages autoloads: %S"
        (require 'al-autoload)
        (al/generate-autoloads al/emacs-my-packages-dir
                               :output-file al/emacs-my-package-autoloads
                               :add-to-path 'prepend
                               :subdirs t)))
    (al/load al/emacs-my-package-autoloads)))


;;; Final settings

;; Settings that cannot be set in other config files because they are
;; loaded before external packages are autoloaded.

(al/title-message "Final settings")

(when (fboundp 'mwim-beginning)
  (if (display-graphic-p)
      (al/bind-keys
        ("C-a" . mwim-beginning)
        ("<ctrl-i>" . mwim-end))
    (al/bind-keys
      ("M-a" . mwim-beginning)
      ("M-i" . mwim-end))))

;; These hooks cannot be set in my init files because my utils, my
;; packages, and external packages are autoloaded after loading the init
;; files.
(al/add-hook-maybe 'after-change-major-mode-hook
  'al/set-default-input-method)
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
    al/hl-todo-mode
    abbrev-mode
    al/set-comment-column
    al/show-trailing-whitespace))
(al/add-hook-maybe 'messages-buffer-mode-hook
  (list 'al/hl-todo-mode
        (lambda () (setq buffer-read-only nil))))
(al/add-after-init-hook 'which-key-mode)
(al/eval-after-init
  ;; Append to make `dim' work for the started shell buffers.
  :append t
  (when (string= server-name "server-emms")
    (appt-activate)
    (al/shells)))

(when (and (fboundp 'smartparens-mode)
           (fboundp 'paredit-mode))
  (al/global-parens-mode))

(when (require 'al-process nil t)
  (advice-add 'insert-directory :around #'al/call-with-locale)
  (al/enable-process-hooks))

(with-demoted-errors "ERROR during server start: %S"
  (require 'al-server)
  (al/server-named-start "server-emms" "server"))

(message "Garbage collected %d times." gcs-done)
(al/title-message "Emacs config has been loaded")

;;; init.el ends here
