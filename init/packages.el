;;; packages.el --- Emacs packages and interfaces to other package systems  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

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

(eval-when-compile
  (require 'al-aux-macros))
(require 'al-places)
(require 'al-general)
(require 'al-key)


;;; Emacs packages

(al/bind-key "H-q" tui/package)

(al/eval-after-load package
  (al/load-settings "package"))

(al/setq-no-warnings
 quelpa-upgrade-p t
 quelpa-dir (al/emacs-data-dir-file "quelpa"))


;;; Guix

(al/bind-key "H-x" guix)
(al/bind-keys
  :prefix-map al/guix-map
  :prefix-docstring "Map for guix."
  :prefix "H-M-x"
  ("H-x" . guix)
  ("f"   . build-farm)
  ("e"   . guix-edit)
  ("b"   . guix-switch-to-buffer)
  ("P"   . guix-prettify-mode)
  ("z"   . guix-switch-to-repl)
  ("C-n" . guix-packages-by-name)
  ("n"   . guix-search-by-name)
  ("r"   . guix-search-by-regexp)
  ("A"   . guix-all-packages)
  ("N"   . guix-newest-packages)
  ("I"   . guix-installed-packages)
  ("O"   . guix-obsolete-packages)
  ("G"   . guix-generations)
  ("a"   . guix-about)
  ("h"   . guix-help)
  ("H"   . guix-hash)
  ("p"   . guix-profiles)
  ("H-p" . guix-set-current-profile)
  ("i"   . al/guix-switch-to-package-info-buffer)
  ("<ctrl-i>" . al/guix-switch-to-generation-info-buffer)
  ("l"   . al/guix-switch-to-package-list-buffer)
  ("C-l" . al/guix-switch-to-generation-list-buffer)
  ("u"   . al/guix-commit-url))

(al/eval-after-load guix
  (al/load-settings "guix"))


;;; Aurel

(al/bind-keys
 :prefix-map al/aurel-map
 :prefix-docstring "Map for aurel."
 :prefix "C-H-a"
 ("i"     . al/switch-to-aurel-info)
 ("l"     . al/switch-to-aurel-list)
 ("C-n"   . aurel-package-info)
 ("p"     . aurel-package-search)
 ("n"     . aurel-package-search)
 ("m"     . aurel-maintainer-search)
 ("I"     . aurel-installed-packages))

(al/eval-after-load aurel
  (al/load-settings "aurel"))

;;; packages.el ends here
