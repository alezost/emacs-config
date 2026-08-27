;;; guix.el --- Settings for `guix' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'guix)
(require 'al-guix)
(require 'al-key)

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
  (al/bind-keys
    :map guix-ui-map
    ("M-P" (message "%s" (guix-ui-current-profile)))))

(al/eval-after-load guix-ui-package
  (setq
   guix-package-list-type 'package)

  (al/bind-keys
    :map guix-package-info-mode-map
    ("M-d" 'guix-package-info-edit)
    ("I"   'guix-package-info-install)
    ("D"   'guix-package-info-delete)
    ("U"   'guix-package-info-upgrade)
    ("S"   'guix-package-info-size))
  (al/bind-keys
    :map guix-package-list-mode-map
    ("M-d" 'guix-package-list-edit)
    ("I"   'guix-package-list-mark-install)
    ("D"   'guix-package-list-mark-delete)
    ("U"   'guix-package-list-mark-upgrade)
    ("S"   'guix-package-list-size))
  (al/bind-keys
    :map guix-output-list-mode-map
    ("M-d" 'guix-output-list-edit)
    ("I"   'guix-output-list-mark-install)
    ("D"   'guix-output-list-mark-delete)
    ("U"   'guix-output-list-mark-upgrade)))

(al/eval-after-load guix-ui-generation
  (setq
   guix-generation-list-show-single t
   guix-generation-packages-update-buffer nil
   guix-generation-output-name-width 40)

  (al/bind-keys
    :map guix-generation-list-mode-map
    ("E" 'guix-generation-list-ediff)
    ("D" 'guix-generation-list-mark-delete)))

(al/eval-after-load guix-ui-service
  (al/bind-keys
    :map guix-service-info-mode-map
    ("M-d" 'guix-service-info-edit))
  (al/bind-keys
    :map guix-service-list-mode-map
    ("M-d" 'guix-service-list-edit)))

(al/eval-after-load guix-ui-package-location
  (al/bind-keys
    :map guix-package-location-list-mode-map
    ("M-d" 'guix-package-location-list-edit)))

(al/eval-after-load guix-ui-service-location
  (al/bind-keys
    :map guix-service-location-list-mode-map
    ("M-d" 'guix-service-location-list-edit)))

(al/eval-after-load guix-ui-license
  (al/bind-keys
    :map guix-license-list-mode-map
    ("M-d" 'guix-license-list-edit)))

(al/eval-after-load guix-ui-store-item
  (al/bind-keys
    :map guix-store-item-list-mode-map
    "R"
    ("M-d" 'guix-store-item-list-edit)
    ("r" 'guix-store-item-list-requisites)
    ("d" 'guix-store-item-list-derivers)
    ("D" 'guix-store-item-list-mark-delete)))

(al/eval-after-load guix-ui-profile
  (setq guix-profiles
        (append guix-profiles
                (al/guix-profiles))))

(al/eval-after-load guix-utils
  (setq
   guix-find-file-function #'org-open-file))

(al/eval-after-load guix-build-log
  (al/bind-keys
    :map guix-build-log-common-map
    ("M-↑" 'guix-build-log-previous-phase)
    ("M-↓" 'guix-build-log-next-phase))
  (al/bind-keys
    :map guix-build-log-mode-map
    ("C-c c" 'compilation-shell-minor-mode) t))

;; TODO `guix-popup' uses `transient' nowadays.
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

;;; guix.el ends here
