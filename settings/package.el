;;; package.el --- Settings for `package' package  -*- lexical-binding: t -*-

(require 'package)
(require 'al-places)
(require 'al-key)
(require 'al-package)
(require 'al-quelpa)

(al/bind-keys
  :map package-menu-mode-map
  ("→" 'package-menu-describe-package)
  ("I" 'package-menu-mark-install)
  ("D" 'package-menu-mark-delete)
  ("^" 'package-menu-mark-upgrades)
  ("z" 'package-menu-mark-unmark))

(setq
 package-archives nil

 al/ignored-packages
 ;; Redundant dependencies of magit:
 '(magit-section)

 al/main-packages
 `((quelpa             :fetcher github :repo "quelpa/quelpa")
   (mwim               :fetcher git :url ,(al/emacs-repo "mwim"))
   (alect-themes       :fetcher git :url ,(al/emacs-repo "alect-themes"))
   (dvorak-layouts     :fetcher git :url ,(al/emacs-repo "dvorak-layouts"))
   (dim                :fetcher git :url ,(al/emacs-repo "dim"))
   (shift-number       :fetcher git :url ,(al/emacs-repo "shift-number"))
   (imenus             :fetcher git :url ,(al/emacs-repo "imenus"))
   paredit
   smartparens
   elisp-slime-nav
   transient
   company
   which-key
   (pathify            :fetcher git :url ,(al/emacs-repo "pathify"))
   (point-pos          :fetcher git :url ,(al/emacs-repo "point-pos"))
   (web-search         :fetcher git :url ,(al/emacs-repo "web-search"))
   (debpaste           :fetcher git :url ,(al/emacs-repo "debpaste"))
   (aurel              :fetcher git :url ,(al/emacs-repo "aurel"))
   (make-color         :fetcher git :url ,(al/emacs-repo "make-color"))
   (date-at-point      :fetcher git :url ,(al/emacs-repo "date-at-point"))
   ;; With the MELPA's 'magit' package recipe, magit repo can be
   ;; downloaded several times to build the magit package itself and
   ;; its dependencies (right now it's only 'magit-section', in the
   ;; past it was more).  So install everything in one piece.
   (magit              :fetcher github :repo "magit/magit"
                       :files ("lisp/*.el" "docs/*.texi"))
   emms
   (emms-mpv           :fetcher git :url ,(al/emacs-repo "emms-mpv"))
   (emms-state         :fetcher git :url ,(al/emacs-repo "emms-state"))
   google-translate
   browse-kill-ring
   browse-at-remote
   markdown-mode
   syslog-mode
   pkgbuild-mode
   list-environment
   pcmpl-args
   geiser
   geiser-guile
   geiser-racket
   sly
   w3m
   (wget               :fetcher github :repo "ataka/emacs-wget")
   agent-shell
   pdf-tools)

 al/extra-packages
 `(outline-magic
   (erc-view-log       :fetcher github :repo "alezost/erc-view-log"
                       :branch "general-regexps")
   (journal            :fetcher git :url ,(al/emacs-repo "journal"))
   (learn-alphabet     :fetcher git :url ,(al/emacs-repo "learn-alphabet"))
   (mana               :fetcher git :url ,(al/emacs-repo "mana"))
   (ducpel             :fetcher git :url ,(al/emacs-repo "ducpel")
                       :files ("*.el"))
   (pretty-sha-path    :fetcher git :url ,(al/emacs-repo "pretty-sha-path"))
   (text-search        :fetcher git :url ,(al/emacs-repo "text-search"))
   (darts-value        :fetcher git :url ,(al/emacs-repo "darts-value"))
   (sokoban            :fetcher github :repo "leoliu/sokoban"
                       :files ("*.el" "sokoban.levels"))
   ;; XXX Quelpa can't install packages from ELPA.  So I have to
   ;; install the following packages manually: `rainbow-mode',
   ;; `url-scgi' (dependency of `mentor').
   mentor))

(advice-add 'package-installed-p :around #'al/package-installed-p)
(advice-add 'quelpa-package-install :around #'al/quelpa-package-install)
(advice-add 'package-compute-transaction :around #'al/package-compute-transaction)
(advice-add 'package-activate-1 :around #'al/package-activate-1)

;;; package.el ends here
