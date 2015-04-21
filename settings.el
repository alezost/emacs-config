;;; settings.el --- Miscellaneous settings

;; Copyright © 2012-2015 Alex Kost

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


;;; Server

(use-package server
  :defer t
  :config
  (setq
   server-kill-new-buffers nil
   server-temp-file-regexp "^/tmp/Re\\|/draft$\\|COMMIT_EDITMSG\\|git-rebase-todo"))

(use-package utl-server
  :init
  (utl-server-named-start '("server-emms" "server")))


;;; Minibuffer, ido, smex

(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local cursor-type 'hbar)))

(al/bind-keys-from-vars 'minibuffer-local-map 'al/minibuffer-keys)

(use-package minibuffer
  :config
  (when (require 'utl-ido nil t)
    (advice-add 'read-file-name-default :around #'utl-ido-disable)))

(use-package ido
  :init (ido-mode t)
  :config
  (setq
   ido-use-virtual-buffers t
   ;; Disable auto searching for files unless called explicitly.
   ido-auto-merge-delay-time 999
   ido-enable-last-directory-history t
   ido-save-directory-list-file (al/emacs-data-dir-file "ido.last")
   ido-record-commands nil
   ido-enable-tramp-completion nil
   ido-enable-flex-matching t
   ido-create-new-buffer 'always
   ido-decorations
   '("\n─► " "" "\n   " "\n   ..." "[" "]" " [No match]" " [Matched]"
     " [Not readable]" " [Too big]" " [Confirm]" "\n─► " " ◄─"))

  ;; Generate ido keymaps only once: `ido-init-completion-maps' is
  ;; called every time an ido command is invoked.  (XXX fixed in 25.1)
  (ido-init-completion-maps)
  (defalias 'ido-init-completion-maps 'ignore)

  (defconst al/ido-common-keys
    '(("C-l"    . ido-toggle-ignore)
      ("C-M-l"  . ido-toggle-regexp)
      ("C-."    . ido-prev-match)
      ("C-e"    . ido-next-match)
      ("<up>"   . ido-prev-match)
      ("<down>" . ido-next-match)
      ("M-d"    . ido-edit-input)
      ("M-k"    . utl-ido-copy-current-item)
      ("M-s"    . ido-select-text)
      "SPC")
    "Alist of auxiliary keys for `ido-common-completion-map'.")
  (al/bind-keys-from-vars
      '(ido-common-completion-map
        ido-file-dir-completion-map
        ido-file-completion-map
        ido-buffer-completion-map)
    '(al/minibuffer-keys al/ido-common-keys))
  (bind-keys
   :map ido-file-dir-completion-map
   ("H-j"   . ido-enter-dired)
   ("C-d"   . ido-fallback-command)
   ("M-."   . ido-prev-work-directory)
   ("M-e"   . ido-next-work-directory)
   ("C-M-." . ido-prev-match-dir)
   ("C-M-e" . ido-next-match-dir)
   ("M-m"   . ido-enter-magit-status)
   ("M-h"   . utl-ido-home-work-directory))
  (bind-keys
   :map ido-buffer-completion-map
   ("C-d"   . ido-fallback-command))

  (al/add-hook-maybe 'ido-minibuffer-setup-hook 'al/no-truncate-lines)
  (when (require 'utl-ido nil t)
    (setq completing-read-function #'utl-completing-read))

  (ido-everywhere))

(use-package smex
  :defer t
  :init
  (setq smex-save-file (al/emacs-data-dir-file "smex-items"))
  (bind-key "M-t" 'execute-extended-command ctl-x-map)
  (bind-key "C-M-t" 'smex-major-mode-commands)
  (bind-key* "M-t" 'smex)
  :config
  (setq
   smex-history-length 32
   smex-prompt-string
        (concat (key-description (where-is-internal 'smex nil t))
                " (smex): "))
  (defun al/smex-prepare-ido-bindings ()
    "Add my bindings to the pseudo smex map."
    (let ((map ido-completion-map))
      (define-key map (kbd "C-h f") 'smex-describe-function)
      (define-key map (kbd "C-h w") 'smex-where-is)
      (define-key map (kbd "M-d")   'smex-find-function)
      (define-key map (kbd "C-d")   'smex-describe-function)))
  (defalias 'smex-prepare-ido-bindings 'al/smex-prepare-ido-bindings))


;;; Working with buffers: ibuffer, uniquify, …

(bind-keys*
 ("M-b" . mode-line-other-buffer)
 ("C-M-b" . ibuffer))

(bind-keys*
 :prefix-map al/buffer-map
 :prefix-docstring "Map for managing/switching to buffers."
 :prefix "C-b"
 ("C-b" . ido-switch-buffer)
 ("r" . rename-buffer)
 ("n" . info)
 ("b" . utl-buffer-name-to-kill-ring)
 ("f" . utl-file-name-to-kill-ring)
 ("g" . (lambda () (interactive) (switch-to-buffer "*grep*")))
 ("o" . (lambda () (interactive) (switch-to-buffer "*Occur*")))
 ("h" . (lambda () (interactive) (switch-to-buffer "*Help*")))
 ("s" . (lambda () (interactive) (switch-to-buffer "*scratch*")))
 ("w" . utl-switch-to-w3m)
 ("m" . man)
 ("k" . (lambda () (interactive) (kill-buffer nil)))
 ("8" . utl-switch-to-characters))

(use-package uniquify
  :defer nil
  :config
  (setq uniquify-buffer-name-style 'post-forward))

(use-package ibuffer
  :defer t
  :config
  (setq ibuffer-default-sorting-mode 'filename/process)
  (al/bind-keys-from-vars 'ibuffer-mode-map)
  (bind-keys
   :map ibuffer-mode-map
   ("u"   . ibuffer-visit-buffer)
   ("."   . ibuffer-backward-line)
   ("e"   . ibuffer-forward-line)
   ("M-." . ibuffer-backward-filter-group)
   ("M-e" . ibuffer-forward-filter-group)

   ("d"   . ibuffer-visit-buffer-other-window-noselect)
   ("C-d" . ibuffer-visit-buffer-other-window)
   ("C-l" . (lambda () (interactive) (ibuffer-update t)))

   ("z"   . ibuffer-unmark-forward)
   ("Z"   . (lambda () (interactive) (ibuffer-unmark-all 0)))
   ("* o" . ibuffer-mark-old-buffers))
  (al/add-hook-maybe 'ibuffer-mode-hook
    '(utl-mode-ibuffer-info hl-line-mode)))


;;; Working with windows and frames

(al/add-hook-maybe 'window-configuration-change-hook
  'utl-set-windows-num-property)

;; Open some buffers in the same window.
(setq same-window-buffer-names
      '("*Colors*" "*Faces*" "*Character List*" "*Character Set List*"
        "*Proced*" "*Google Translate*" "*Help*" "*Apropos" "*Messages*"
        "*Personal Keybindings*" "*YASnippet tables*" "*Occur*"))
(push ".*\\.el\\.gz$" same-window-regexps)

(bind-keys
 ("<H-XF86AudioRaiseVolume>"   . (lambda () (interactive) (enlarge-window 1)))
 ("<H-XF86AudioLowerVolume>"   . (lambda () (interactive) (enlarge-window -1)))
 ("<C-H-XF86AudioRaiseVolume>" . (lambda () (interactive) (enlarge-window 1 t)))
 ("<C-H-XF86AudioLowerVolume>" . (lambda () (interactive) (enlarge-window -1 t)))
 ("H-o" . other-window)
 ("H-M-o" . utl-switch-windows)
 ("H-M-q" . (lambda () (interactive)
              (quit-window nil (previous-window))))
 ("H-O" . utl-switch-to-minibuffer)
 ("H-1" . delete-other-windows)
 ("H-2" . utl-make-vertical-windows)
 ("H-3" . utl-make-horizontal-windows))

(use-package winner
  :disabled t
  :defer 5
  :init
  (setq winner-dont-bind-my-keys t)
  (bind-keys
   ("<C-left>"  . winner-undo)
   ("<C-right>" . winner-redo))
  :config
  (setq winner-ring-size 40)
  (winner-mode))


;;; comint, shell, eshell

(setq shell-file-name "bash")

(bind-keys*
 :prefix-map al/repl-map
 :prefix-docstring "Map for various REPLs."
 :prefix "C-n"
 ("C-s" . shell)
 ("t"   . visit-ansi-term)
 ("e"   . eshell)
 ("i"   . ielm)
 ("s"   . (lambda () (interactive) (al/sql-connect 'darts)))
 ("l"   . slime-repl)
 ("g"   . (lambda (arg) (interactive "P")
            (let (geiser-repl-use-other-window)
              (switch-to-guile arg))))
 ("G"   . (lambda () (interactive)
            (geiser-connect
             'guile "localhost" guix-default-port)))
 ("P"   . run-python)
 ("p"   . python-shell-switch-to-shell)
 ("L"   . lua-start-process)
 ("m"   . maxima))

(use-package comint
  :defer t
  :config
  (defconst al/comint-keys
    '(("M-." . comint-previous-input)
      ("M-e" . comint-next-input)
      ("M->" . comint-previous-prompt)
      ("M-E" . comint-next-prompt)
      ("C-c c" . compilation-shell-minor-mode)
      "C-d")
    "Alist of auxiliary keys for comint modes.")
  (al/bind-keys-from-vars 'comint-mode-map 'al/comint-keys))

(use-package eshell
  :defer t
  :init
  (setq eshell-directory-name (al/emacs-data-dir-file "eshell"))
  (bind-keys
   ("C-z"   . eshell)
   ("C-M-z" . utl-eshell-cd))
  :config
  (setq
   eshell-modules-list
   '(eshell-smart eshell-alias eshell-basic eshell-cmpl eshell-dirs
     eshell-glob eshell-hist eshell-ls eshell-pred eshell-prompt
     eshell-script eshell-term eshell-unix eshell-tramp)
   eshell-highlight-prompt nil
   eshell-hist-ignoredups t
   eshell-history-size 9999)

  (defconst al/eshell-keys
    '(("<M-tab>" . eshell-complete-lisp-symbol)
      ("C-a" . eshell-bol)
      ("C-k" . utl-eshell-kill-whole-line)
      ("M-." . eshell-previous-input)
      ("M-e" . eshell-next-input)
      ("M->" . eshell-previous-prompt)
      ("M-E" . eshell-next-prompt)
      ("M-r" . utl-eshell-previous-matching-input-from-input)
      ("M-s" . utl-eshell-next-matching-input-from-input))
    "Alist of auxiliary keys for `eshell-mode'.")
  ;; For some strange reason `eshell-mode-map' is buffer local, so key
  ;; bindings should be put in a hook.
  (defun al/eshell-bind-keys ()
    (al/bind-keys-from-vars 'eshell-mode-map
      '(al/free-misc-keys al/eshell-keys)))

  ;; Default value of `paragraph-separate' breaks
  ;; `eshell-next-prompt'/`eshell-previous-prompt'.
  (defun al/eshell-set-paragraph ()
    (setq-local paragraph-separate "useLESS var"))

  (al/add-hook-maybe 'eshell-mode-hook
    '(al/eshell-bind-keys al/eshell-set-paragraph))

  (require 'tramp nil t)
  (when (require 'utl-eshell nil t)
    (setq
     eshell-prompt-function 'utl-eshell-prompt
     eshell-prompt-regexp utl-eshell-prompt-regexp)
    (defalias 'eshell/info 'utl-eshell/info)))


;;; Button, custom, widget

(use-package button
  :defer t
  :config
  (al/bind-keys-from-vars 'button-buffer-map 'al/button-keys t)
  (bind-key "u" 'push-button button-map))

(use-package wid-edit
  :defer t
  :config
  (defconst al/widget-button-keys
    '(("." . widget-backward)
      ("e" . widget-forward)
      ("u" . widget-button-press))
    "Alist of auxiliary keys for modes with widget buttons.")
  (defconst al/widget-field-keys
    '(("M-<" . widget-kill-line)
      ("C-п" . widget-end-of-line)
      ("C-k"   (beginning-of-line) (widget-kill-line)))
    "Alist of auxiliary keys for modes with widget fields.")
  (al/bind-keys-from-vars 'widget-keymap 'al/widget-button-keys t)
  (al/bind-keys-from-vars 'widget-field-keymap 'al/widget-field-keys))

(use-package cus-edit
  :defer t
  :config
  (al/bind-keys-from-vars 'custom-mode-map 'al/widget-button-keys t)
  (bind-keys
   :map custom-mode-map
   ("o" . Custom-goto-parent)
   ("g" . Custom-reset-standard)))


;;; Help, apropos, man, info

(setq apropos-do-all t)

(use-package help
  :defer t
  :config
  (bind-keys
   :map help-map
   ("R" . (lambda () (interactive) (info "elisp")))
   ("A" . apropos)))

(use-package help-mode
  :defer t
  :config
  (bind-keys
   :map help-mode-map
   ("," . help-go-back)
   ("p" . help-go-forward))
  (al/add-hook-maybe 'help-mode-hook 'al/no-truncate-lines))

(use-package man
  :defer t
  :config
  (setq Man-notify-method 'pushy)
  (when (require 'utl-mode-line nil t)
    (utl-mode-line-default-buffer-identification 'Man-mode))

  (defconst al/man-keys
    '(("h" . Man-previous-section)
      ("n" . Man-next-section)
      ("m" . Man-goto-section)
      ("g" . Man-update-manpage))
    "Alist of auxiliary keys for `Man-mode'.")
  (al/bind-keys-from-vars 'Man-mode-map
    '(al/button-keys al/man-keys)))

(use-package info
  :defer t
  :config
  (setq Info-additional-directory-list
        (list (al/guix-user-profile-dir-file "share/info/")))
  (bind-keys
   :map Info-mode-map
   ("." . Info-prev-reference)
   ("e" . Info-next-reference)
   ("c" . (lambda () (interactive) (Info-copy-current-node-name 0)))
   ("o" . (lambda () (interactive) (Info-up) (goto-char (point-min))))
   ("O" . Info-top-node)
   ("u" . Info-follow-nearest-node)
   ("," . Info-history-back)
   ("p" . Info-history-forward)
   ("y" . Info-history)
   ("k" . Info-index-next)
   ("h" . Info-prev)
   ("n" . Info-next)
   ("H" . Info-help)))


;;; SQL

(use-package sql
  :defer t
  :init
  (setq
   sql-product 'mysql
   sql-database "darts"
   sql-user user-login-name)
  (defun al/sql-connect (conn)
    (let ((sql-connection-alist
           `((darts (sql-product 'mysql)
                    (sql-server "")
                    (sql-database "darts")
                    (sql-user ,user-login-name)
                    (sql-password ,(utl-sql-password-from-auth-source
                                    "sql-darts" user-login-name)))
             (paste (sql-product 'postgres)
                    (sql-server "")
                    (sql-database "paste")
                    (sql-user "www-data")
                    (sql-password ,(utl-sql-password-from-auth-source
                                    "sql-paste"))))))
      (sql-connect conn)
      (setq sql-password nil)))

  :config
  (bind-keys
   :map sql-mode-map
   ("C-v"   . sql-send-region)
   ("C-M-v" . sql-send-paragraph)
   ("M-s-v" . sql-send-buffer))
  (bind-key "<tab>" 'complete-symbol sql-interactive-mode-map)

  (when (require 'utl-sql nil t)
    (defalias 'sql-highlight-product 'utl-sql-highlight-product)
    (al/add-hook-maybe 'sql-interactive-mode-hook
      '(utl-sql-save-history
        utl-sql-highlight-product
        utl-sql-completion-setup)))

  ;; Fix bug with mariadb prompt:
  ;; <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17426>.
  (sql-set-product-feature 'mysql :prompt-regexp
                           "^\\(?:mysql\\|mariadb\\).*> "))

(use-package mysql
  :defer t
  :config
  (setq mysql-user sql-user)
  (when (require 'utl-mysql nil t)
    (defalias 'mysql-shell-query 'utl-mysql-shell-query)))

(use-package sql-completion
  :defer t
  :config
  (setq
   sql-mysql-database sql-database
   sql-mysql-exclude-databases
   '("mysql" "information_schema" "performance_schema"))
  (require 'cl nil t))

(use-package utl-sql
  :defer t
  :config
  (setq utl-sql-history-dir (al/emacs-data-dir-file "sql")))


;;; Darts, journal

(bind-keys
 :prefix-map al/darts-map
 :prefix-docstring "Map for darts and journal."
 :prefix "M-D"
 ("d" . darts-day-template)
 ("s" . darts-day-select)
 ("e" . darts-day-export)
 ("M-S M-D" . journal-search-by-date)
 ("M-S M-S" . journal-grep)
 ("j" . journal-create-entry)
 ("w" . journal-position-windows)
 ("c" . journal-change-created-property)
 ("v" . journal-change-converted-property)
 ("b" . journal-change-described-property)
 ("h" . journal-insert-subheading)
 ("H" . journal-back-to-entry-heading)
 ("i" . journal-insert-block)
 ("t" . (lambda () (interactive)
          (find-file (al/journal-dir-file "tags")))))

(use-package journal
  :defer t
  :init (al/add-my-package-to-load-path-maybe "journal")
  :config
  (setq
   org-id-files (al/with-check
                  :dir al/journal-dir
                  (directory-files al/journal-dir t
                                   journal-file-name-regexp))
   org-id-locations-file (al/emacs-data-dir-file "org-id-locations")
   org-id-track-globally t
   org-agenda-files org-id-files
   journal-current-file (car (last org-id-files)))
  (setq
   journal-directory     al/journal-dir
   journal-template-file (al/journal-dir-file "template"))
  (setq
   journal-open-block "┃"
   journal-close-block "┃")
  (defun al/journal-no-double-space ()
    (and (journal-buffer-p)
         (setq-local sentence-end-double-space nil)))
  (al/add-hook-maybe 'org-mode-hook 'al/journal-no-double-space))

(use-package darts-value
  :defer t
  :commands (darts-throw-string-to-points darts-throw-string-to-code)
  :init (al/add-my-package-to-load-path-maybe "darts-value"))

(use-package darts-daydata
  :defer t
  :commands (darts-day-template darts-day-select)
  :init (al/add-my-package-to-load-path-maybe "darts-daydata")
  :config
  (setq
   darts-database "darts"
   darts-data-dir "~/darts/daytables"
   darts-exported-dir (expand-file-name "exported" darts-data-dir)
   darts-template-file (expand-file-name "template" darts-data-dir)))


;;; Initial scratch and message buffers

(setq
 initial-major-mode 'ignore
 initial-buffer-choice #'messages-buffer
 message-log-max 5000)

(defun al/set-scratch-message ()
  (setq initial-scratch-message
        (format (concat ";; Started: %s\n"
                        ";; Init time: %s\n\n")
                (format-time-string "%d %B, %A %T" before-init-time)
                (emacs-init-time))))

(defun al/reinit-messages-buffer ()
  "Initialize `messages-buffer-mode-hook' in a message buffer."
  (with-current-buffer (messages-buffer)
    (messages-buffer-mode)))

(al/add-hook-maybe 'after-init-hook
  '(al/set-scratch-message al/reinit-messages-buffer))

(al/add-hook-maybe 'messages-buffer-mode-hook
  (list 'hl-todo-mode
        (lambda () (setq buffer-read-only nil))))


;;; Misc settings and packages

(setq
 gc-cons-threshold 4000000
 password-cache-expiry (* 24 60 60)
 echo-keystrokes 0.2
 disabled-command-function nil
 inhibit-startup-screen t)

(electric-indent-mode 0)

(defalias 'yes-or-no-p 'y-or-n-p)

(al/bind-keys-from-vars 'special-mode-map 'al/lazy-moving-keys t)

(use-package tramp-sh
  :defer t
  :config
  (push "/run/current-system/profile/bin" tramp-remote-path)
  (push "LC_ALL=en_US.UTF-8" tramp-remote-process-environment)
  (push "DISPLAY=:0" tramp-remote-process-environment))

(use-package gnutls
  :defer t
  :config
  ;; http://comments.gmane.org/gmane.emacs.gnus.general/83413
  (setq gnutls-min-prime-bits nil))

(use-package picture
  :defer t
  :config
  (bind-keys
   :map picture-mode-map
   ("M-O" . picture-movement-left)
   ("M-U" . picture-movement-right)
   ("M->" . picture-movement-up)
   ("M-E" . picture-movement-down)
   ("M-<" . picture-movement-nw)
   ("M-P" . picture-movement-ne)
   ("M-Q" . picture-movement-sw)
   ("M-K" . picture-movement-se)))

(use-package hexl
  :defer t
  :config
  (bind-keys
   :map hexl-mode-map
   ("C-." . hexl-previous-line)
   ("C-e" . hexl-next-line)
   ("C-o" . hexl-backward-char)
   ("C-u" . hexl-forward-char)
   ("M-o" . hexl-backward-short)
   ("M-u" . hexl-forward-short)
   ("C-i" . hexl-end-of-line)
   ("H-." . hexl-scroll-down)
   ("H-e" . hexl-scroll-up)
   ("H-a" . hexl-beginning-of-buffer)
   ("H-i" . hexl-end-of-buffer)))

(use-package ediff
  :defer t
  :config
  (when (require 'utl-ediff nil t)
    (al/add-hook-maybe 'ediff-before-setup-hook
      'utl-ediff-save-window-configuration)
    (al/add-hook-maybe 'ediff-quit-hook
      'utl-ediff-restore-window-configuration
      t))
  (setq
   ediff-window-setup-function #'ediff-setup-windows-plain ; no new frame
   ediff-split-window-function #'split-window-horizontally
   ediff-grab-mouse nil)

  ;; The way `ediff-mode' works with the key bindings is even more evil
  ;; than `eshell-mode' does.
  (defconst al/ediff-keys
    '(("h" . ediff-previous-difference)
      ("H" . ediff-toggle-hilit))
    "Alist of auxiliary keys for `ediff-mode-map'.")
  (defun al/ediff-bind-keys ()
    (al/bind-keys-from-vars 'ediff-mode-map 'al/ediff-keys))
  (al/add-hook-maybe 'ediff-startup-hook 'al/ediff-bind-keys))

(use-package view
  :defer t
  :diminish " 👀"
  :config
  (bind-keys
   :map view-mode-map
   ("f" . View-exit))
  (al/bind-keys-from-vars 'view-mode-map 'al/lazy-moving-keys t))

(use-package epa
  :defer t
  :config
  (require 'wid-edit) ; for `al/widget-button-keys' (it is required anyway)
  (al/bind-keys-from-vars 'epa-key-list-mode-map
    'al/widget-button-keys t)
  (bind-keys
   :map epa-key-list-mode-map
   ("z" . epa-unmark-key)))

(use-package etags
  :defer t
  :init
  (bind-keys
   :prefix-map al/tags-map
   :prefix-docstring "Map for tags."
   :prefix "M-T"
   ("M-T" . find-tag)
   ("d"   . (lambda () (interactive)
              (find-tag (find-tag-default))))
   ("r"   . find-tag-regexp)
   ("n"   . tags-loop-continue)
   ("v"   . visit-tags-table)
   ("c"   . utl-create-tags))
  :config
  (setq tags-file-name (al/src-dir-file "conkeror/modules/TAGS")))

;;; settings.el ends here
