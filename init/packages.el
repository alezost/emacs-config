;;; packages.el --- Emacs packages and interfaces to other package systems

;; Copyright © 2014-2016 Alex Kost

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

(require 'al-key)


;;; Emacs packages

(al/bind-keys
 :prefix-map al/package-map
 :prefix-docstring "Map for Emacs packages commands."
 :prefix "H-q"
 ("H-q" . al/quelpa)
 ("q"     (let (quelpa-update-melpa-p)
            (call-interactively #'quelpa)))
 ("l"   . al/switch-to-packages)
 ("a"   . al/add-package-archive)
 ("r"   . al/remove-package-archive))

(with-eval-after-load 'package
  (setq package-archives nil)
  (al/bind-keys
   :map package-menu-mode-map
   ("u" . package-menu-describe-package)
   ("i" . package-menu-mark-install)
   ("d" . package-menu-mark-delete)
   ("z" . package-menu-mark-unmark)))


;;; Guix

(setq
 guix-current-profile al/guix-user-profile-dir
 guix-directory (al/src-dir-file "guix"))
(require 'guix-autoloads nil t)

(al/bind-keys
 :prefix-map al/guix-map
 :prefix-docstring "Map for guix."
 :prefix "H-x"
 ("H-x" . guix)
 ("e"   . guix-edit)
 ("b"   . guix-build-log-minor-mode)
 ("B"   . guix-build-log-mode)
 ("P"   . guix-prettify-mode)
 ("z"   . guix-switch-to-repl)
 ("C-n" . guix-packages-by-name)
 ("n"   . guix-search-by-name)
 ("r"   . guix-search-by-regexp)
 ("A"   . guix-all-available-packages)
 ("N"   . guix-newest-available-packages)
 ("I"   . guix-installed-packages)
 ("O"   . guix-obsolete-packages)
 ("G"   . guix-generations)
 ("p"   . guix-set-current-profile)
 ("i"     (switch-to-buffer (guix-package-info-buffer-name
                             guix-current-profile)))
 ("C-п"   (switch-to-buffer (guix-generation-info-buffer-name
                             guix-current-profile)))
 ("l"     (switch-to-buffer (guix-package-list-buffer-name
                             guix-current-profile)))
 ("C-l"   (switch-to-buffer (guix-generation-list-buffer-name
                             guix-current-profile)))
 ("u"   . al/guix-commit-url))

(with-eval-after-load 'guix-external
  (setq guix-guile-program "guile"))

(with-eval-after-load 'guix-base
  (setq
   guix-operation-option-separator "  │  ")
  (when (display-graphic-p)
    (setq
     guix-operation-option-false-string "☐"
     guix-operation-option-true-string  "☑")))

(with-eval-after-load 'guix-buffer
  (defconst al/guix-buffer-keys
    '(("," . guix-history-back)
      ("p" . guix-history-forward))
    "Alist of auxiliary keys for `guix-buffer-map'.")
  (al/bind-keys-from-vars 'guix-buffer-map 'al/guix-buffer-keys t))

(with-eval-after-load 'guix-list
  (defconst al/guix-list-keys
    '(("u" . guix-list-describe)
      ("z" . guix-list-unmark)
      ("Z" . guix-list-unmark-all))
    "Alist of auxiliary keys for `guix-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-list-mode-map 'al/guix-list-keys t)
  (defconst al/guix-list-key-vars
    '(al/lazy-moving-keys
      al/tabulated-list-keys
      al/guix-buffer-keys
      al/guix-list-keys)))

(with-eval-after-load 'guix-ui
  (defconst al/guix-ui-keys
    '(("P"   (message "%s" (guix-ui-current-profile))))
    "Alist of auxiliary keys for `guix-ui-map'.")
  (al/bind-keys-from-vars 'guix-ui-map 'al/guix-ui-keys t))

(with-eval-after-load 'guix-ui-package
  (setq
   guix-package-list-type 'package)

  (defconst al/guix-package-list-keys
    '(("M-d" . guix-package-list-edit))
    "Alist of auxiliary keys for `guix-package-list-mode-map'.")
  (defconst al/guix-output-list-keys
    '(("M-d" . guix-output-list-edit))
    "Alist of auxiliary keys for `guix-output-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-package-list-mode-map
    (append al/guix-list-key-vars '(al/guix-package-list-keys))
    t)
  (al/bind-keys-from-vars 'guix-output-list-mode-map
    (append al/guix-list-key-vars '(al/guix-output-list-keys))
    t))

(with-eval-after-load 'guix-ui-generation
  (setq
   guix-generation-packages-update-buffer nil
   guix-generation-output-name-width 40)

  (defconst al/guix-generation-list-keys
    '(("E" . guix-generation-list-ediff))
    "Alist of auxiliary keys for `guix-generation-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-generation-list-mode-map
    (append al/guix-list-key-vars '(al/guix-generation-list-keys))
    t))

(with-eval-after-load 'guix-utils
  (setq
   guix-find-file-function #'org-open-file))

(with-eval-after-load 'guix-prettify
  (setq
   guix-prettify-regexp (rx "/" (or "store" "nar" "log")
                            "/" (group (= 32 alnum)))
   guix-prettify-regexp-group 1))
(al/add-after-init-hook 'global-guix-prettify-mode)

(with-eval-after-load 'guix-build-log
  (setq guix-build-log-minor-mode-activate nil)
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

(with-eval-after-load 'guix-devel
  (defconst al/guix-devel-keys
    '(("d" . guix-devel-download-package-source))
    "Alist of auxiliary keys for `guix-devel-keys-map'.")
  (al/bind-keys-from-vars 'guix-devel-keys-map 'al/guix-devel-keys))


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

(with-eval-after-load 'aurel
  (setq
   aurel-download-directory (al/src-dir-file "abs")
   aurel-aur-user-name "alezost"
   aurel-list-buffer-name "*aur-list*"
   aurel-info-buffer-name "*aur-info*"
   aurel-date-format "%d-%b-%Y %T"
   aurel-empty-string "–"
   aurel-aur-user-package-info-check t
   aurel-info-aur-user-string "——————————————————————————————————————\n"
   aurel-info-installed-package-string aurel-info-aur-user-string
   aurel-debug-level 9
   aurel-list-column-format
   '((name 20 t)
     (version 9 t)
     (installed-version 9 t)
     (maintainer 13 t)
     (votes 5 aurel-list-sort-by-votes)
     (description 30 nil))
   aurel-list-column-name-alist
   '((votes . "V.")
     (version . "Ver.")
     (installed-version . "Inst.")))

  (al/bind-keys
   :map aurel-list-mode-map
   ("u" . aurel-list-describe-package)
   ("," . aurel-history-back)
   ("p" . aurel-history-forward)
   ("z" . aurel-list-unmark)
   ("Z" . aurel-list-unmark-all))
  (al/bind-keys
   :map aurel-info-mode-map
   ("," . aurel-history-back)
   ("p" . aurel-history-forward)))

;;; packages.el ends here
