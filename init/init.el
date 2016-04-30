;;; init.el --- Init file   -*- lexical-binding: t -*-

;; Copyright © 2012-2016 Alex Kost

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

(setq load-prefer-newer t)

(defvar al/emacs-trunk?
  (not (version< emacs-version "25.0.50"))
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

(defun al/guix-profile (name)
  "Return file name of my guix profile with NAME."
  (al/guix-profile-dir-file (concat name "/" name)))

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

(defun al/guix-set-load-path (dir)
  (al/with-check
    :dir dir
    (al/add-to-load-path-maybe dir)
    (setq guix-load-path dir)))
(al/guix-set-load-path (al/devel-dir-file "guix/emacs"))
(al/guix-set-load-path (al/src-dir-file "guix/emacs"))

(setq
 package-user-dir (al/emacs-data-dir-file "elpa")
 package-enable-at-startup nil
 guix-package-enable-at-startup nil)

(unless al/pure-config?
  (with-demoted-errors "ERROR during autoloading ELPA packages: %S"
    (package-initialize))
  (with-demoted-errors "ERROR during autoloading Guix packages: %S"
    (when (require 'guix-emacs nil t)
      (guix-emacs-autoload-packages (al/guix-profile "emacs"))))
  (when (file-exists-p al/emacs-my-packages-dir)
    (with-demoted-errors "ERROR during autoloading my packages: %S"
      (let ((dirs (al/subdirs al/emacs-my-packages-dir)))
        (setq load-path (append dirs load-path))
        (dolist (dir dirs)
          (al/load (al/autoloads-file dir)))))))

(defun al/package-installed-p (fun package &rest args)
  "Do not check the version of a built-in package.
Some built-in packages (e.g., `org', `erc') do not have 'Version'
header field.  This may break things if a third-party package
relies on a particular version of a built-in package (e.g.,
'org-6.1' or 'erc-5.3').  So just ignore the version."
  (or (package-built-in-p package)
      (apply fun package args)))

(advice-add 'package-installed-p :around #'al/package-installed-p)


;;; Ignoring packages (dependencies)

(defvar al/ignored-packages
  '(
    ;; Installed via Guix:
    pdf-tools
    emms
    geiser
    magit
    ;; Redundant dependencies of magit:
    magit-popup git-commit with-editor)
  "Names of packages that shouldn't be installed.")

(defun al/remove-ignored-packages (requirements)
  "Remove `al/ignored-packages' from the REQUIREMENTS."
  (cl-remove-if (lambda (req)
                  (memq (car req) al/ignored-packages))
                requirements))

;; FIXME In a better world advising `package-desc-reqs' should work, but
;; it doesn't, presumably because it is `cl-defsubst' generated by
;; (cl-defstruct (package-desc ...)).  Is there a workaround?

;; (defun al/package-desc-reqs (fun desc &rest args)
;;   "Return requirements without `al/ignored-packages'."
;;   (let ((reqs (apply fun desc args)))
;;     (al/remove-ignored-packages reqs)))

;; (advice-add 'package-desc-reqs :around #'al/package-desc-reqs)

;; Since the above won't work, I have to mess with modifying other
;; functions:
;;
;; - `quelpa-package-install' and `package-compute-transaction': to
;; avoid building/installing unneeded dependencies;
;;
;; - `package-generate-description-file' and `pb/write-pkg-file': to
;; remove unneeded dependencies from a generated "…-pkg.el" file, thus
;; to make sure startup activation will not complain about missing
;; packages.

(defun al/quelpa-package-install (fun package &rest args)
  "Do not install PACKAGE if it is one of `al/ignored-packages'."
  (let* ((name (al/package-name package))
         (ignore? (memq name al/ignored-packages)))
    (if ignore?
        (message "Ignoring '%s' package." name)
      (apply fun package args))))

(advice-add 'quelpa-package-install
  :around #'al/quelpa-package-install)

(defun al/package-compute-transaction (fun packages requirements
                                           &rest args)
  "Reduce REQUIREMENTS by excluding `al/ignored-packages'."
  (apply fun packages
         (al/remove-ignored-packages requirements)
         args))

(advice-add 'package-compute-transaction
  :around #'al/package-compute-transaction)

(defun al/package-generate-description-file (fun pkg-desc pkg-file
                                                 &rest args)
  "Reduce requirements from PKG-DESC."
  (setf (package-desc-reqs pkg-desc)
        (al/remove-ignored-packages (package-desc-reqs pkg-desc)))
  (apply fun pkg-desc pkg-file args))

(advice-add 'package-generate-description-file
  :around #'al/package-generate-description-file)

(defun al/package-build--write-pkg-file (fun pkg-file pkg-info
                                             &rest args)
  "Reduce requirements from PKG-INFO."
  (let ((new-reqs (al/remove-ignored-packages (aref pkg-info 1))))
    (aset pkg-info 1 new-reqs)
    (apply fun pkg-file pkg-info args)))

(advice-add 'package-build--write-pkg-file
  :around #'al/package-build--write-pkg-file)


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
(al/define-package-exists mwim mwim-beginning-of-code-or-line)


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
