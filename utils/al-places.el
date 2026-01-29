;;; al-places.el --- Location of various files  -*- lexical-binding: t -*-

;; Copyright © 2012–2026 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU Places Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU Places Public License for more details.
;;
;; You should have received a copy of the GNU Places Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(defmacro al/file-accessors (name val)
  "Define variable and function for accessing my directories."
  (declare (indent 1))
  (let ((dir-var  (intern (concat "al/" name "-dir")))
        (file-fun (intern (concat "al/" name "-dir-file"))))
    `(progn
       (defvar ,dir-var ,val)
       (defun ,file-fun (file)
         ,(format "Return full file name of a FILE placed in `%s'."
                  dir-var)
         (expand-file-name file ,dir-var)))))

(al/file-accessors "emacs-utils" (file-name-directory
                                  (file-truename load-file-name)))
(al/file-accessors "emacs"      (al/emacs-utils-dir-file "../"))
(al/file-accessors "emacs-data" (al/emacs-dir-file "data"))
(al/file-accessors "emacs-init" (al/emacs-dir-file "init"))
(al/file-accessors "emacs-my-packages" (al/emacs-dir-file "packages"))

(al/file-accessors "config"     "~/config")
(al/file-accessors "notes"      "~/notes")
(al/file-accessors "progs"      "~/progs")
(al/file-accessors "journal"    (al/notes-dir-file "journal"))
(al/file-accessors "music"      "~/music")
(al/file-accessors "sound"      "~/docs/audio/small")
(al/file-accessors "tmp"        "~/tmp")
(al/file-accessors "src"        "~/src")
(al/file-accessors "devel"      "~/devel")
(al/file-accessors "download"   "~/downloads")
(al/file-accessors "math"       "~/maths")


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

(provide 'al-places)

;;; al-places.el ends here
