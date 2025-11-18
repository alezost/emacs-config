;;; al-guix-autoload.el --- Additional functionality to autoload Guix packages  -*- lexical-binding: t -*-

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

(require 'cl-lib)
(require 'seq)
(require 'al-autoload)
(require 'al-file)

(defvar al/guix-emacs-autoloads nil
  "List of the last loaded Emacs autoloads.")

(defun al/guix-emacs-directory (profile)
  "Return directory with Emacs packages installed in PROFILE."
  (expand-file-name "share/emacs/site-lisp" profile))

(defun al/guix-emacs-directories (profile)
  "Return list of directories under PROFILE that contain Emacs packages.
This includes both `share/emacs/site-lisp/guix.d/PACKAGE'
sub-directories and `share/emacs/site-lisp' itself.
Return nil, if Emacs packages are not installed in PROFILE."
  (let ((root-dir (al/guix-emacs-directory profile)))
    (when (file-directory-p root-dir)
      (let* ((pkgs-dir  (expand-file-name "guix.d" root-dir))
             (pkgs-dirs (when (file-directory-p pkgs-dir)
                          (al/subdirs pkgs-dir))))
        (cons root-dir pkgs-dirs)))))

(defun al/guix-autoload-emacs-packages (&rest profiles)
  "Autoload Emacs packages installed in Guix PROFILES."
  (dolist (profile profiles)
    (let ((dirs (al/guix-emacs-directories profile)))
      (when dirs
        (let* ((autoloads     (seq-keep #'al/find-autoloads dirs))
               (new-autoloads (seq-difference autoloads
                                              al/guix-emacs-autoloads
                                              #'string=)))
          (dolist (dir dirs)
            (cl-pushnew (directory-file-name dir)
                        load-path
                        :test #'string=))
          (dolist (autoload new-autoloads)
            (load autoload 'noerror))
          (setq al/guix-emacs-autoloads
                (append new-autoloads al/guix-emacs-autoloads)))))))

(provide 'al-guix-autoload)

;;; al-guix-autoload.el ends here
