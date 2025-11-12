;;; packages.el --- Emacs packages and interfaces to other package systems

;; Copyright © 2014–2022 Alex Kost

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
   ("I" . package-menu-mark-install)
   ("D" . package-menu-mark-delete)
   ("^" . package-menu-mark-upgrades)
   ("z" . package-menu-mark-unmark)))

(setq
 quelpa-upgrade-p t
 ;; Quelpa dirs are used in several places of my config.
 quelpa-dir (al/emacs-data-dir-file "quelpa")
 quelpa-build-dir (expand-file-name "build" quelpa-dir))

(with-eval-after-load 'al-quelpa
  (defun al/emacs-repo (name)
    "Return git url of a repository with my package NAME."
    (concat "https://gitlab.com/alezost-emacs/" name ".git"))

  (setq
   al/main-packages
   `((quelpa             :fetcher github :repo "quelpa/quelpa")
     (mwim               :fetcher git :url ,(al/emacs-repo "mwim"))
     (alect-themes       :fetcher git :url ,(al/emacs-repo "alect-themes"))
     (dvorak-layouts     :fetcher git :url ,(al/emacs-repo "dvorak-layouts"))
     (dim                :fetcher git :url ,(al/emacs-repo "dim"))
     (insert-pair        :fetcher git :url ,(al/emacs-repo "insert-pair"))
     (imenus             :fetcher git :url ,(al/emacs-repo "imenus"))
     smex
     smartparens
     elisp-slime-nav
     hydra
     (yasnippet          :fetcher github :repo "capitaomorte/yasnippet"
                         :files ("yasnippet.el"))

     (shift-number       :fetcher git :url ,(al/emacs-repo "shift-number"))
     (pathify            :fetcher git :url ,(al/emacs-repo "pathify"))
     (point-pos          :fetcher git :url ,(al/emacs-repo "point-pos"))
     (web-search         :fetcher git :url ,(al/emacs-repo "web-search"))
     (text-search        :fetcher git :url ,(al/emacs-repo "text-search"))
     (darts-value        :fetcher git :url ,(al/emacs-repo "darts-value"))
     (debpaste           :fetcher git :url ,(al/emacs-repo "debpaste"))
     (aurel              :fetcher git :url ,(al/emacs-repo "aurel"))
     (make-color         :fetcher git :url ,(al/emacs-repo "make-color"))
     (pretty-sha-path    :fetcher git :url ,(al/emacs-repo "pretty-sha-path"))
     (date-at-point      :fetcher git :url ,(al/emacs-repo "date-at-point"))
     (journal            :fetcher git :url ,(al/emacs-repo "journal"))
     pcmpl-args
     org-emms
     (dictem             :fetcher github :repo "cheusov/dictem")
     google-translate
     (emms-state         :fetcher git :url ,(al/emacs-repo "emms-state"))
     browse-kill-ring
     browse-at-remote
     outline-magic
     markdown-mode
     syslog-mode
     list-environment
     (mysql              :fetcher github :repo "haxney/mysql")
     (sql-completion     :fetcher github :repo "emacsmirror/sql-completion")
     erc-hl-nicks
     (erc-view-log       :fetcher github :repo "alezost/erc-view-log"
                         :branch "general-regexps")
     hl-todo)

   al/extra-packages
   `(indent-guide
     mentor
     pkgbuild-mode

     ;; With the MELPA's 'magit' package recipe, magit repo will be
     ;; downloaded 4 times to build the magit package itself and its
     ;; dependencies (git-commit, magit-popup and with-editor).  So
     ;; install everything in one piece.
     (magit              :fetcher github :repo "magit/magit"
                         :files ("lisp/*.el" "Documentation/*.texi"))

     (rainbow-mode       :fetcher url :url "http://git.savannah.gnu.org/cgit/emacs/elpa.git/plain/packages/rainbow-mode/rainbow-mode.el")
     (typing-practice    :fetcher url :url "https://raw.github.com/mebubo/dotfiles/master/.emacs.d/site-lisp/typing-practice.el")
     (mana               :fetcher git :url ,(al/emacs-repo "mana"))
     (ducpel             :fetcher git :url ,(al/emacs-repo "ducpel")
                         :files ("*.el"))
     typing-game
     (sokoban            :fetcher github :repo "leoliu/sokoban"
                         :files ("*.el" "sokoban.levels")))))


;;; Guix

(setq guix-current-profile al/guix-user-profile-dir)

(let ((dir (al/devel-dir-file "guix")))
  (when (file-exists-p dir)
    (setq guix-load-path dir)))

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

(defconst al/guix-list-keys
  '(("i" . bui-list-describe)
    ("S" . guix-package-list-size))
  "Alist of auxiliary keys for guix list maps.")

(defconst al/guix-list-key-vars
  '(al/lazy-moving-keys
    al/tabulated-list-keys
    al/bui-list-keys
    al/guix-list-keys))

(with-eval-after-load 'guix-external
  (setq guix-guile-program "guile"))

(with-eval-after-load 'guix-repl
  (when (require 'al-geiser nil t)
    (defun al/geiser-add-guix-socket ()
      (cl-pushnew guix-repl-current-socket al/geiser-sockets
                  :test #'string=))
    (add-hook 'guix-repl-after-start-hook 'al/geiser-add-guix-socket)
    (remove-hook 'guix-repl-after-operation-hook
                 'guix-repl-autoload-emacs-packages-maybe)))

(with-eval-after-load 'guix-misc
  (setq
   guix-operation-option-separator "  │  ")
  (when (display-graphic-p)
    (setq
     guix-operation-option-false-string "☐"
     guix-operation-option-true-string  "☑")))

(with-eval-after-load 'guix-ui
  (defconst al/guix-ui-keys
    '(("M-P" (message "%s" (guix-ui-current-profile))))
    "Alist of auxiliary keys for `guix-ui-map'.")
  (al/bind-keys-from-vars 'guix-ui-map 'al/guix-ui-keys t))

(with-eval-after-load 'guix-ui-package
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

(with-eval-after-load 'guix-ui-generation
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

(with-eval-after-load 'guix-ui-service
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

(with-eval-after-load 'guix-ui-package-location
  (defconst al/guix-package-location-list-keys
    '(("M-d" . guix-package-location-list-edit))
    "Alist of auxiliary keys for `guix-package-location-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-package-location-list-mode-map
    (append al/guix-list-key-vars '(al/guix-package-location-list-keys))
    t))

(with-eval-after-load 'guix-ui-service-location
  (defconst al/guix-service-location-list-keys
    '(("M-d" . guix-service-location-list-edit))
    "Alist of auxiliary keys for `guix-service-location-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-service-location-list-mode-map
    (append al/guix-list-key-vars '(al/guix-service-location-list-keys))
    t))

(with-eval-after-load 'guix-ui-license
  (defconst al/guix-license-list-keys
    '(("M-d" . guix-license-list-edit))
    "Alist of auxiliary keys for `guix-license-list-mode-map'.")
  (al/bind-keys-from-vars 'guix-license-list-mode-map
    (append al/guix-list-key-vars '(al/guix-license-list-keys))
    t))

(with-eval-after-load 'guix-ui-store-item
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

(with-eval-after-load 'guix-ui-profile
  (setq guix-profiles
        (append guix-profiles
                (al/guix-profiles))))

(with-eval-after-load 'guix-utils
  (setq
   guix-find-file-function #'org-open-file))

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

(with-eval-after-load 'guix-popup
  ;; Use "P" for packages and "p" for profiles.
  (magit-change-popup-key 'guix-popup :action ?p ?–)
  (magit-change-popup-key 'guix-popup :action ?P ?p)
  (magit-change-popup-key 'guix-popup :action ?– ?P)
  (when (require 'al-magit-popup nil t)
    (al/magit-add-popup-keys
     'guix-popup :action
     '((?z "switch to REPL" guix-switch-to-repl)
       (?u "browse commit URL" al/guix-commit-url)
       (?f "build farm" build-farm)))))


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
   aurel-aur-user-package-info-check t
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
