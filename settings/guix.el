;;; guix.el --- Settings for `guix' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'guix)
(require 'al-guix)
(require 'al-key)

(defconst al/guix-list-keys
  '(("i" . bui-list-describe)
    ("S" . guix-package-list-size)))

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
    '(("M-P" (message "%s" (guix-ui-current-profile)))))
  (al/bind-keys-from-vars 'guix-ui-map 'al/guix-ui-keys t))

(al/eval-after-load guix-ui-package
  (setq
   guix-package-list-type 'package)

  (defconst al/guix-package-info-keys
    '(("M-d" . guix-package-info-edit)
      ("I"   . guix-package-info-install)
      ("D"   . guix-package-info-delete)
      ("U"   . guix-package-info-upgrade)
      ("S"   . guix-package-info-size)))
  (defconst al/guix-package-list-keys
    '(("M-d" . guix-package-list-edit)
      ("I"   . guix-package-list-mark-install)
      ("D"   . guix-package-list-mark-delete)
      ("U"   . guix-package-list-mark-upgrade)))
  (defconst al/guix-output-list-keys
    '(("M-d" . guix-output-list-edit)
      ("I"   . guix-output-list-mark-install)
      ("D"   . guix-output-list-mark-delete)
      ("U"   . guix-output-list-mark-upgrade)))
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
      ("D" . guix-generation-list-mark-delete)))
  (al/bind-keys-from-vars 'guix-generation-list-mode-map
    (append al/guix-list-key-vars '(al/guix-generation-list-keys))
    t))

(al/eval-after-load guix-ui-service
  (defconst al/guix-service-info-keys
    '(("M-d" . guix-service-info-edit)))
  (defconst al/guix-service-list-keys
    '(("M-d" . guix-service-list-edit)))
  (al/bind-keys-from-vars 'guix-service-info-mode-map
    '(al/button-keys al/guix-service-info-keys)
    t)
  (al/bind-keys-from-vars 'guix-service-list-mode-map
    (append al/guix-list-key-vars '(al/guix-service-list-keys))
    t))

(al/eval-after-load guix-ui-package-location
  (defconst al/guix-package-location-list-keys
    '(("M-d" . guix-package-location-list-edit)))
  (al/bind-keys-from-vars 'guix-package-location-list-mode-map
    (append al/guix-list-key-vars '(al/guix-package-location-list-keys))
    t))

(al/eval-after-load guix-ui-service-location
  (defconst al/guix-service-location-list-keys
    '(("M-d" . guix-service-location-list-edit)))
  (al/bind-keys-from-vars 'guix-service-location-list-mode-map
    (append al/guix-list-key-vars '(al/guix-service-location-list-keys))
    t))

(al/eval-after-load guix-ui-license
  (defconst al/guix-license-list-keys
    '(("M-d" . guix-license-list-edit)))
  (al/bind-keys-from-vars 'guix-license-list-mode-map
    (append al/guix-list-key-vars '(al/guix-license-list-keys))
    t))

(al/eval-after-load guix-ui-store-item
  (defconst al/guix-store-item-list-keys
    '("R"
      ("M-d" . guix-store-item-list-edit)
      ("r" . guix-store-item-list-requisites)
      ("d" . guix-store-item-list-derivers)
      ("D" . guix-store-item-list-mark-delete)))
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
      ("M-e" . guix-build-log-next-phase)))
  (defconst al/guix-build-log-keys
    '(("C-c c" . compilation-shell-minor-mode)))
  (al/bind-keys-from-vars 'guix-build-log-common-map
    'al/guix-build-log-common-keys)
  (al/bind-keys-from-vars 'guix-build-log-mode-map
    'al/guix-build-log-keys t))

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
