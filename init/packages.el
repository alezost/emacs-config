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

(declare-function al/display-buffer "al-buffer")


;;; Emacs packages

(al/bind-key "H-q" tui/package)

(al/eval-after-load package
  (al/load-settings "package"))

(al/setq-no-warnings
 quelpa-upgrade-p t
 quelpa-dir (al/emacs-data-dir-file "quelpa"))


;;; Guix

(defconst al/guix-list-keys
  '(("i" . bui-list-describe)
    ("S" . guix-package-list-size))
  "Alist of auxiliary keys for guix list maps.")

(defconst al/guix-list-key-vars
  '(al/lazy-moving-keys
    al/tabulated-list-keys
    al/bui-list-keys
    al/guix-list-keys))

(al/eval-after-load guix-profiles
  (setq guix-current-profile al/guix-user-profile-dir))

(al/eval-after-load guix-external
  (setq guix-guile-program "guile"))

(defvar al/geiser-sockets)
(al/eval-after-load guix-repl
  (al/setq-file guix-load-path (al/devel-dir-file "guix"))

  (when (al/require al-geiser)
    (al/eval-at-hook guix-repl-after-start-hook
      :once t
      (push al/geiser-sockets guix-repl-current-socket))
    (remove-hook 'guix-repl-after-operation-hook
                 'guix-repl-autoload-emacs-packages-maybe)))

(al/eval-after-load guix-misc
  (setq
   guix-operation-option-separator "  │  ")
  (when (display-graphic-p)
    (setq
     guix-operation-option-false-string "☐"
     guix-operation-option-true-string  "☑")))

(al/eval-after-load guix-ui
  (defconst al/guix-ui-keys
    '(("M-P" (message "%s" (guix-ui-current-profile))))
    "Alist of auxiliary keys for `guix-ui-map'.")
  (al/bind-keys-from-vars 'guix-ui-map 'al/guix-ui-keys t))

(al/eval-after-load guix-ui-package
  (setq
   guix-package-list-type 'package)

  (defconst al/guix-package-info-keys
    '(("M-d" . guix-package-info-edit)
      ("I"   . guix-package-info-install)
      ("D"   . guix-package-info-delete)
      ("U"   . guix-package-info-upgrade)
      ("S"   . guix-package-info-size))
    "Alist of auxiliary keys for `guix-package-info-mode-map'.")
  (defconst al/guix-package-list-keys
    '(("M-d" . guix-package-list-edit)
      ("I"   . guix-package-list-mark-install)
      ("D"   . guix-package-list-mark-delete)
      ("U"   . guix-package-list-mark-upgrade))
    "Alist of auxiliary keys for `guix-package-list-mode-map'.")
  (defconst al/guix-output-list-keys
    '(("M-d" . guix-output-list-edit)
      ("I"   . guix-output-list-mark-install)
      ("D"   . guix-output-list-mark-delete)
      ("U"   . guix-output-list-mark-upgrade))
    "Alist of auxiliary keys for `guix-output-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-package-info-mode-map
    '(al/button-keys al/guix-package-info-keys)
    t)
  (al/bind-keys-from-vars 'guix-package-list-mode-map
    (append al/guix-list-key-vars '(al/guix-package-list-keys))
    t)
  (al/bind-keys-from-vars 'guix-output-list-mode-map
    (append al/guix-list-key-vars '(al/guix-output-list-keys))
    t))

(al/eval-after-load guix-ui-generation
  (setq
   guix-generation-list-show-single t
   guix-generation-packages-update-buffer nil
   guix-generation-output-name-width 40)

  (defconst al/guix-generation-list-keys
    '(("E" . guix-generation-list-ediff)
      ("D" . guix-generation-list-mark-delete))
    "Alist of auxiliary keys for `guix-generation-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-generation-list-mode-map
    (append al/guix-list-key-vars '(al/guix-generation-list-keys))
    t))

(al/eval-after-load guix-ui-service
  (defconst al/guix-service-info-keys
    '(("M-d" . guix-service-info-edit))
    "Alist of auxiliary keys for `guix-service-info-mode-map'.")
  (defconst al/guix-service-list-keys
    '(("M-d" . guix-service-list-edit))
    "Alist of auxiliary keys for `guix-service-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-service-info-mode-map
    '(al/button-keys al/guix-service-info-keys)
    t)
  (al/bind-keys-from-vars 'guix-service-list-mode-map
    (append al/guix-list-key-vars '(al/guix-service-list-keys))
    t))

(al/eval-after-load guix-ui-package-location
  (defconst al/guix-package-location-list-keys
    '(("M-d" . guix-package-location-list-edit))
    "Alist of auxiliary keys for `guix-package-location-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-package-location-list-mode-map
    (append al/guix-list-key-vars '(al/guix-package-location-list-keys))
    t))

(al/eval-after-load guix-ui-service-location
  (defconst al/guix-service-location-list-keys
    '(("M-d" . guix-service-location-list-edit))
    "Alist of auxiliary keys for `guix-service-location-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-service-location-list-mode-map
    (append al/guix-list-key-vars '(al/guix-service-location-list-keys))
    t))

(al/eval-after-load guix-ui-license
  (defconst al/guix-license-list-keys
    '(("M-d" . guix-license-list-edit))
    "Alist of auxiliary keys for `guix-license-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-license-list-mode-map
    (append al/guix-list-key-vars '(al/guix-license-list-keys))
    t))

(al/eval-after-load guix-ui-store-item
  (defconst al/guix-store-item-list-keys
    '("R"
      ("M-d" . guix-store-item-list-edit)
      ("r" . guix-store-item-list-requisites)
      ("d" . guix-store-item-list-derivers)
      ("D" . guix-store-item-list-mark-delete))
    "Alist of auxiliary keys for `guix-store-item-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-store-item-list-mode-map
    (append al/guix-list-key-vars '(al/guix-store-item-list-keys))
    t))

(al/eval-after-load guix-ui-profile
  (setq guix-profiles
        (append guix-profiles
                (al/guix-profiles))))

(al/eval-after-load guix-utils
  (setq
   guix-find-file-function #'org-open-file))

(al/eval-after-load guix-build-log
  (defconst al/guix-build-log-common-keys
    '(("M-." . guix-build-log-previous-phase)
      ("M-e" . guix-build-log-next-phase))
    "Alist of auxiliary keys for `guix-build-log-common-map'.")
  (defconst al/guix-build-log-keys
    '(("C-c c" . compilation-shell-minor-mode))
    "Alist of auxiliary keys for `guix-build-log-mode-map'.")
  (al/bind-keys-from-vars 'guix-build-log-common-map
    'al/guix-build-log-common-keys)
  (al/bind-keys-from-vars 'guix-build-log-mode-map
    'al/guix-build-log-keys t))

;; TODO `guix-popup' uses `transient' now.
;;
;; (al/eval-after-load guix-popup
;;   ;; Use "P" for packages and "p" for profiles.
;;   (magit-change-popup-key 'guix-popup :action ?p ?–)
;;   (magit-change-popup-key 'guix-popup :action ?P ?p)
;;   (magit-change-popup-key 'guix-popup :action ?– ?P)
;;   (when (al/require al-magit-popup)
;;     (al/magit-add-popup-keys
;;      'guix-popup :action
;;      '((?z "switch to REPL" guix-switch-to-repl)
;;        (?u "browse commit URL" al/guix-commit-url)
;;        (?f "build farm" build-farm)))))

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
  ("i"     (al/display-buffer (guix-package-info-buffer-name
                               guix-current-profile)))
  ("<ctrl-i>" (al/display-buffer (guix-generation-info-buffer-name
                                  guix-current-profile)))
  ("l"     (al/display-buffer (guix-package-list-buffer-name
                               guix-current-profile)))
  ("C-l"   (al/display-buffer (guix-generation-list-buffer-name
                               guix-current-profile)))
  ("u"   . al/guix-commit-url))


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
  (setq
   aurel-download-directory (al/src-dir-file "abs")
   aurel-aur-user-name "alezost"
   ;; aurel-aur-user-package-info-check t
   aurel-info-aur-user-string "——————————————————————————————————————\n"
   aurel-info-installed-package-string aurel-info-aur-user-string
   aurel-debug-level 9
   aurel-list-format
   '((name aurel-list-get-name 20 t)
     (version nil 9 t)
     (installed-version nil 9 t)
     (maintainer aurel-list-get-maintainer 13 t)
     (votes nil 8 bui-list-sort-numerically-4 :right-align t)
     (description nil 30 nil))
   aurel-list-titles
   '((votes . "V.")
     (version . "Ver.")
     (installed-version . "Inst.")))
  (defconst al/aurel-filter-keys
    ;; Default `aurel-enable-filter' was left for backward compatibility.
    '(("f" . bui-enable-filter))
    "Alist of auxiliary keys for `aurel-filter-map'.")
  (al/bind-keys-from-vars 'aurel-filter-map 'al/aurel-filter-keys))

;;; packages.el ends here
