;;; init.el --- Init file  -*- lexical-binding: t -*-

;; Copyright © 2012-2016 Alex Kost

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

(setq load-prefer-newer t)

(defvar al/emacs-trunk?
  (not (version< emacs-version "26.0.50"))
  "Non-nil, if current Emacs is the latest development build.")


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
(al/file-accessors "echo-download" (al/download-dir-file "echo"))


;;; Required utils

(push al/emacs-utils-dir load-path)
(require 'al-autoload)
(require 'al-file)
(require 'al-misc)
(require 'al-text)

(defun al/init-load (file)
  "Load FILE from `al/emacs-init-dir'."
  (al/load (al/emacs-init-dir-file file)))


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


;;; Autoloading utils

(let ((auto-file (al/autoloads-file al/emacs-utils-dir)))
  (unless (file-exists-p auto-file)
    (with-demoted-errors "ERROR during generating utils autoloads: %S"
      (al/update-autoloads al/emacs-utils-dir)))
  (al/load auto-file))


;;; Server

(with-demoted-errors "ERROR during server start: %S"
  (require 'al-server)
  (al/server-named-start "server-emms" "server"))


;;; External packages

(defvar al/pure-config? (getenv "EMPURE")
  "Non-nil, if external packages should not be loaded.")

(setq
 package-user-dir (al/emacs-data-dir-file "elpa")
 package-enable-at-startup nil
 guix-package-enable-at-startup nil)

(unless al/pure-config?
  (with-demoted-errors "ERROR during autoloading ELPA packages: %S"
    (when (require 'al-package nil t)
      (setq
       al/ignored-packages
       '( ;; Installed via Guix:
         pdf-tools
         bui
         dash
         emms
         geiser
         magit
         ;; Redundant dependencies of magit:
         magit-popup git-commit with-editor))

      (advice-add 'package-installed-p
        :around #'al/package-installed-p)
      (advice-add 'quelpa-package-install
        :around #'al/quelpa-package-install)
      (advice-add 'package-compute-transaction
        :around #'al/package-compute-transaction)
      (advice-add 'package-generate-description-file
        :around #'al/package-generate-description-file)
      (advice-add 'package-build--write-pkg-file
        :around #'al/package-build--write-pkg-file))
    (package-initialize))
  (with-demoted-errors "ERROR during autoloading Guix packages: %S"
    (when (require 'al-guix-autoload nil t)
      (apply #'al/guix-autoload-emacs-packages
             (mapcar #'al/guix-profile
                     '("emacs" "main" "misc")))))
  (when (file-exists-p al/emacs-my-packages-dir)
    (with-demoted-errors "ERROR during autoloading my packages: %S"
      (dolist (dir (al/subdirs al/emacs-my-packages-dir))
        (let* ((elisp-dir (expand-file-name "elisp" dir))
               (dir (if (file-exists-p elisp-dir)
                        elisp-dir
                      dir)))
          (push dir load-path)
          (mapc #'al/load (al/find-autoloads dir)))))))


;;; Code for optional dependencies on external packages

(defmacro al/define-package-exists (name &optional symbol)
  "Define `al/NAME-exists?' variable.
The value of the variable tells if SYMBOL is `fbound'.  If SYMBOL
is not specified, NAME is checked (both should be unquoted
symbols)."
  (let* ((name-str (symbol-name name))
         (var (intern (concat "al/" name-str "-exists?"))))
    `(defvar ,var (fboundp ',(or symbol name))
       ,(format "Non-nil, if `%s' package is available."
                name-str))))

(al/define-package-exists hydra defhydra)
(al/define-package-exists mwim mwim-beginning)


;;; Loading the rest config

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
        "games"))

;; (setq custom-file "/tmp/custom.el")
(setq custom-file (al/emacs-init-dir-file "custom.el"))
(al/eval-after-init (load custom-file 'noerror))

;;; init.el ends here
