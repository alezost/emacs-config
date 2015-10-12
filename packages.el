;;; packages.el --- Emacs packages and interfaces to other package systems

;; Copyright © 2014-2015 Alex Kost

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


;;; Emacs packages

(bind-keys
 :prefix-map al/package-map
 :prefix-docstring "Map for Emacs packages commands."
 :prefix "H-p"
 ("H-p" . al/quelpa)
 ("q"   . (lambda () (interactive)
            (let (quelpa-update-melpa-p)
              (call-interactively #'quelpa))))
 ("l"   . utl-switch-to-packages)
 ("a"   . utl-add-package-archive)
 ("r"   . utl-remove-package-archive))

(use-package package
  :defer t
  :config
  (setq package-archives nil)
  (bind-keys
   :map package-menu-mode-map
   ("u" . package-menu-describe-package)
   ("i" . package-menu-mark-install)
   ("d" . package-menu-mark-delete)
   ("z" . package-menu-mark-unmark)))


;;; Guix

(bind-keys
 :prefix-map al/guix-map
 :prefix-docstring "Map for guix."
 :prefix "H-x"
 ("H-x" . guix)
 ("e"   . guix-edit)
 ("z"   . guix-switch-to-repl)
 ("C-n" . guix-search-by-name)
 ("n"   . (lambda (regexp)
            (interactive
             (list (read-string "Package name by regexp: "
                                nil 'guix-search-history)))
            (guix-search-by-regexp regexp '(name))))
 ("r"   . guix-search-by-regexp)
 ("A"   . guix-all-available-packages)
 ("N"   . guix-newest-available-packages)
 ("I"   . guix-installed-packages)
 ("O"   . guix-obsolete-packages)
 ("G"   . guix-generations)
 ("p"   . guix-set-current-profile)
 ("i"   . (lambda () (interactive)
            (switch-to-buffer guix-package-info-buffer-name)))
 ("C-п" . (lambda () (interactive)
            (switch-to-buffer guix-generation-info-buffer-name)))
 ("l"   . (lambda () (interactive)
            (switch-to-buffer guix-package-list-buffer-name)))
 ("C-l" . (lambda () (interactive)
            (switch-to-buffer guix-generation-list-buffer-name))))

(use-package guix
  :defer t
  :init
  (defconst al/guix-common-keys
    '(("," . guix-history-back)
      ("p" . guix-history-forward)
      ("P"   (message "%s" guix-profile)))
    "Alist of auxiliary keys that should be bound in any guix mode.")

  (let ((dir (al/devel-dir-file "guix/emacs")))
    (al/add-to-load-path-maybe dir)
    (setq guix-load-path dir))
  ;; (al/add-to-load-path-maybe (al/src-dir-file "guix/emacs"))
  (setq guix-default-profile
        (concat "/var/guix/profiles/per-user/"
                user-login-name "/guix-profile"))
  (require 'guix-init nil t))

(use-package guix-base
  :defer t
  :config
  (setq
   guix-directory (al/src-dir-file "guix")
   guix-package-list-type 'package
   guix-generation-packages-update-buffer nil
   guix-buffer-name-function #'guix-buffer-name-simple
   guix-operation-option-separator "  │  ")
  (when (display-graphic-p)
    (setq
     guix-operation-option-false-string "☐"
     guix-operation-option-true-string  "☑"))

  (al/bind-keys-from-vars 'guix-root-map 'al/guix-common-keys t))

(use-package guix-utils
  :defer t
  :config
  (setq
   guix-find-file-function #'org-open-file))

(use-package guix-info
  :defer t
  :config
  (setq
   guix-package-info-heading-params '(synopsis)
   guix-package-info-fill-heading nil))

(use-package guix-list
  :defer t
  :config
  (setcdr (assq 'package guix-list-column-format)
          '((name 20 t)
            (version 10 t)
            (outputs 13 t)
            (installed 12 t)
            (synopsis 30 t)))
  (setcdr (assq 'output guix-list-column-format)
          '((name 20 t)
            (version 10 t)
            (output 9 t)
            (installed 12 t)
            (synopsis 30 t)))

  (defconst al/guix-list-keys
    '(("u" . guix-list-describe)
      ("z" . guix-list-unmark)
      ("Z" . guix-list-unmark-all))
    "Alist of auxiliary keys for `guix-list-mode-map'.")
  (defconst al/guix-package-or-output-list-keys
    '(("M-d" . guix-list-edit-package))
    "Alist of auxiliary keys for `guix-package-list-mode-map' and
    `guix-output-list-mode-map'.")
  (defconst al/guix-output-list-keys
    '(("u" . guix-output-list-describe))
    "Alist of auxiliary keys for `guix-output-list-mode-map'.")
  (defconst al/guix-generation-list-keys
    '(("u" . guix-generation-list-show-packages)
      ("c" . guix-generation-list-switch)
      ("E" . guix-generation-list-ediff))
    "Alist of auxiliary keys for `guix-generation-list-mode-map'.")

  (al/bind-keys-from-vars 'guix-list-mode-map 'al/guix-list-keys t)
  (let ((list-vars '(al/lazy-moving-keys
                     al/tabulated-list-keys
                     al/guix-common-keys
                     al/guix-list-keys)))
    (al/bind-keys-from-vars 'guix-generation-list-mode-map
      (append list-vars '(al/guix-generation-list-keys))
      t)
    (al/bind-keys-from-vars 'guix-package-list-mode-map
      (append list-vars '(al/guix-package-or-output-list-keys))
      t)
    (al/bind-keys-from-vars 'guix-output-list-mode-map
      (append list-vars '(al/guix-package-or-output-list-keys
                          al/guix-output-list-keys))
      t)))

(use-package guix-prettify
  :defer 7
  :config
  (setq
   guix-prettify-regexp (rx (or "store" "nar" "log")
                            "/" (group (= 32 alnum)))
   guix-prettify-regexp-group 1)
  (global-guix-prettify-mode))

(use-package guix-build-log
  :defer t
  :diminish (guix-build-log-minor-mode . " γ🄻")
  :config
  (setq guix-build-log-mode-hook '(view-mode))
  (defconst al/guix-build-log-keys
    '(("M-." . guix-build-log-previous-phase)
      ("M-e" . guix-build-log-next-phase)
      ("C-c c" . compilation-shell-minor-mode))
    "Alist of auxiliary keys for `guix-build-log-mode-map'.")
  (al/bind-keys-from-vars 'guix-build-log-mode-map
    'al/guix-build-log-keys))


;;; Aurel

(use-package aurel
  :defer t
  :init
  (al/add-my-package-to-load-path-maybe "aurel")
  (bind-keys
   :prefix-map al/aurel-map
   :prefix-docstring "Map for aurel."
   :prefix "C-H-a"
   ("i"     . utl-switch-to-aurel-info)
   ("l"     . utl-switch-to-aurel-list)
   ("C-n"   . aurel-package-info)
   ("p"     . aurel-package-search)
   ("n"     . aurel-package-search)
   ("m"     . aurel-maintainer-search)
   ("I"     . aurel-installed-packages))

  :config
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

  (bind-keys
   :map aurel-list-mode-map
   ("u" . aurel-list-describe-package)
   ("," . aurel-history-back)
   ("p" . aurel-history-forward)
   ("z" . aurel-list-unmark)
   ("Z" . aurel-list-unmark-all))
  (bind-keys
   :map aurel-info-mode-map
   ("," . aurel-history-back)
   ("p" . aurel-history-forward)))

;;; packages.el ends here
