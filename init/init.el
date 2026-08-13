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

(defvar al/emacs-utils-autoloads
  (al/emacs-utils-dir-file "utils-autoloads.el")
  "`autoloads' file for my utils.")
(defvar al/emacs-my-packages-autoloads
  (al/emacs-data-dir-file "my-autoloads.el")
  "`autoloads' file for my packages.")
(defvar al/emacs-elpa-packages-autoloads
  (al/emacs-data-dir-file "elpa-autoloads.el")
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

(al/load-init
 "keys"
 "text"
 "packages"
 "settings"
 "files"
 "prog"
 "time"
 "file-modes"
 "net"
 "visual"
 "games"
 "custom")

(al/title-message (concat "Loading \"autoloads\" files"))

(defvar al/autoloads-presets
  `(("my utils"
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

(dolist (args al/autoloads-presets)
  (apply #'al/load-autoloads args))

;; Prepend paths added by the above autoloads to `load-path' in reverse
;; order.  So the first loaded autoloads have precedence over the last
;; ones.
(setq load-path
      (nconc (apply #'nconc (nreverse al/load-paths))
             al/initial-load-path))

(message "Garbage collected %d times." gcs-done)
(al/title-message "Emacs config has been loaded")

;;; init.el ends here
