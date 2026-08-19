;;; settings.el --- Miscellaneous settings  -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'al-aux-macros))

(require 'al-places)
(require 'al-general)
(require 'al-key)


;;; Minibuffer and completions

(al/bind-key* "M-t" execute-extended-command)

(setq
 completion-show-help nil
 completion-auto-select 'second-tab
 completion-styles '(basic substring partial-completion emacs22)
 completions-sort 'historical
 completions-format 'one-column
 completions-header-format (propertize "%s completions:\n" 'face 'alect-title)
 completions-max-height 20
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t
 enable-recursive-minibuffers t)

(al/call-at-hook minibuffer-setup-hook al/hbar-cursor-type)
(al/bind-keys-from-vars 'minibuffer-local-map 'al/minibuffer-keys)

;; (al/call-after-init icomplete-vertical-mode)

;; Enabling `icomplete-vertical-mode' manually to avoid loading
;; `icomplete' on Emacs start and load it only when minibuffer is used
;; for the first time.

(al/autoload "icomplete"
  icomplete-minibuffer-setup
  icomplete--vertical-minibuffer-setup)
(al/eval-after-init
  (setq icomplete-mode t
        icomplete-vertical-mode t)
  (al/call-at-hook minibuffer-setup-hook
    icomplete-minibuffer-setup)
  (al/call-at-hook icomplete-minibuffer-setup-hook
    icomplete--vertical-minibuffer-setup))

(al/bind-keys
  :map completion-list-mode-map
  ("." . previous-completion)
  ("e" . next-completion))

(al/eval-after-load al-complete
  :load after-init
  (setq
   completion-styles '(al/split)
   completion-ignored-extensions
   '("./" "../"
     ".o" ".bin" ".lbin" ".so" ".a" ".la" ".lo"
     ".elc" ".go" ".pyc"))

  ;; This breaks company completions in SLY buffers
  ;; (advice-add 'completion--styles :override #'al/completion-styles)

  (advice-add 'completion-all-completions :around #'al/completion-all-completions))

(al/eval-after-load al-minibuffer
  :load after-init
  (al/bind-keys
    :map al/minibuffer-buffer-map
    ("M-m" . al/minibuffer-magit-buffers)
    ("M-s" . al/minibuffer-shell-buffers))
  (al/bind-keys
    :map al/minibuffer-file-map
    ("M-h"   (al/minibuffer-set-directory "~"))
    ("M-m" . al/minibuffer-enter-magit-status)
    ("RET" . icomplete-fido-ret)
    ("DEL" . icomplete-fido-backward-updir))
  (al/bind-keys
    :map al/minibuffer-symbol-map
    ("C-d" . al/minibuffer-describe-symbol)
    ("M-d" . al/minibuffer-find-symbol))

  (advice-add 'read-file-name             :around #'al/read-file-add-keymap)
  (advice-add 'read-buffer                :around #'al/read-buffer-add-keymap)
  (advice-add 'read-extended-command      :around #'al/read-symbol-add-keymap)
  (advice-add 'read-face-name             :around #'al/read-symbol-add-keymap)
  (advice-add 'help-fns--describe-function-or-command-prompt ; used by `describe-function'
    :around #'al/read-symbol-add-keymap)

  (advice-add 'find-file                :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'switch-to-buffer         :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'execute-extended-command :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'describe-function        :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'describe-variable        :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'describe-face            :around #'al/minibuffer-fallback-or-funcall)
  (advice-add 'describe-symbol          :around #'al/minibuffer-fallback-or-funcall))

(al/define-multi-command al/tab
  al/tempo-complete-maybe
  al/indent-maybe
  company-complete)

(al/bind-keys
  ("TAB" . al/tab)
  ("<backtab>" . completion-at-point)
  ("<M-tab>" . al/complete-elisp-symbol))

(al/eval-settings-after-load
  (icomplete "icomplete")
  (pcomplete "pcomplete")
  (pcmpl-args
   (setq
    pcmpl-args-debug-parse-help t
    pcmpl-args-cache-default-duration 999999
    pcmpl-args-cache-max-duration pcmpl-args-cache-default-duration))
  (company "company"))


;;; Working with buffers: ibuffer, uniquify, …

(al/eval-after-load al-buffer-cmd
  (al/bind-keys
    :map al/switch-buffer-map
    ("M-b" . al/switch-to-other-buffer)
    ("M-N" . al/switch-to-next-buffer)
    ("M-B" . al/switch-to-prev-buffer)))

(al/bind-keys*
  ("M-b" . al/switch-to-previous-buffer)
  ("C-M-b" (al/find-file (al/notes-dir-file "bookmarks.org"))))

(al/bind-keys*
 :prefix-map al/buffer-map
 :prefix-doc "Map for managing/switching to buffers."
 :prefix-key "C-b"
 ("C-b" . switch-to-buffer)
 ("M-b" . ibuffer)
 ("r" . rename-buffer)
 ("c" . clone-buffer)
 ("n" . info)
 ("b" . al/buffer-name-to-kill-ring)
 ("f" . al/file-name-to-kill-ring)
 ("g"   (al/display-buffer "*grep*"))
 ("o"   (al/display-buffer "*Occur*"))
 ("h"   (al/display-buffer "*Help*"))
 ("s"   (al/display-buffer "*scratch*"))
 ("P" . list-processes)
 ("E" . list-environment)
 ("e" . emoji-list)
 ("F" . al/switch-to-faces)
 ("w" . al/switch-to-w3m)
 ("m" . man)
 ("M" . woman)
 ("k"   (kill-buffer nil))
 ("8" . al/switch-to-characters))

(al/eval-settings-after-load
  (uniquify (setq uniquify-buffer-name-style 'post-forward))
  (ibuffer "ibuffer"))


;;; Working with windows and frames

(setq split-width-threshold 120)

(al/call-at-hook window-configuration-change-hook
  al/set-windows-num-property)

(defvar al/display-buffer-regexp
  (rx (or "*Apropos"
          "*Character List*"
          "*Character Set List*"
          "*Colors*"
          "*Diff*"
          "*Faces*"
          "*Google Translate*"
          "*Help*"
          "*Messages*"
          "*Occur*"
          "*Personal Keybindings*"
          "*Proced*"
          "*Process"
          "*Shadows*"
          "*magit:"))
  "Regexp for buffers that should be displayed specially.")

(setq display-buffer-alist
      `(;; Open some buffers in the same window.
        (,al/display-buffer-regexp
         (display-buffer-reuse-window
          display-buffer-same-window))))

(al/bind-keys
 ("<H-XF86AudioRaiseVolume>"   (enlarge-window 1 t))
 ("<H-XF86AudioLowerVolume>"   (enlarge-window -1 t))
 ("<M-H-XF86AudioRaiseVolume>" (enlarge-window 1))
 ("<M-H-XF86AudioLowerVolume>" (enlarge-window -1))
 ("H-o" . al/other-window)
 ("H-M-o" . al/switch-or-next-window)
 ("H-M-q" (quit-window nil (previous-window)))
 ("H-O" . al/switch-to-minibuffer)
 ("H-1" . delete-other-windows)
 ("H-2" . al/make-vertical-windows)
 ("H-3" . al/make-horizontal-windows))

(al/bind-keys
 :map ctl-x-map
 ("o"   . al/other-window)
 ("M-o" . other-window))


;;; comint, shell, eshell

(setq shell-file-name "bash")

(al/bind-keys
  ("s-s"   . al/shell)
  ("C-z"   . al/eshell)
  ("C-M-z" . al/eshell-cd))

(al/bind-keys*
 :prefix-map al/repl-map
 :prefix-doc "Map for various REPLs."
 :prefix-key "C-n"
 ("C-s" . al/switch-to-shell-buffer)
 ("t"   . visit-ansi-term)
 ("e"   . eshell)
 ("i"   . ielm)
 ("a"   . al/agent-shell)
 ("s"   . al/sql-switch-or-connect)
 ("l"   . al/sly)
 ("L"   . al/sly-connect)
 ("g"   . al/geiser-guile-switch-current-window)
 ("G"   . al/geiser-socket-connect)
 ("h"   . al/haskell-interactive-switch-or-start)
 ("P"   . run-python)
 ("p"   . python-shell-switch-to-shell)
 ("m"   . maxima)
 ("x"   . guix-switch-to-repl))

(al/eval-settings-after-load
  (comint "comint")
  (shell "shell")
  (esh-mode "eshell")
  (agent-shell "agent-shell"))


;;; Button, custom, widget

(al/autoload "bui-button" bui-button-copy-label)

(al/eval-after-load button
  (defconst al/button-map-keys
    '(("u" . push-button)
      ("c" . bui-button-copy-label))
    "Alist of auxiliary keys for `button-map'.")
  (al/bind-keys-from-vars 'button-map 'al/button-map-keys t)
  (al/bind-keys-from-vars 'button-buffer-map 'al/button-keys t))

(al/eval-settings-after-load
  (wid-edit "wid-edit")
  (cus-edit "cus-edit"))


;;; Help, apropos, man, info

(setq help-window-keep-selected t)

(al/bind-keys
  :map help-map
  ("v" . al/describe-variable)
  ("s" . al/describe-symbol)
  ("x" . describe-syntax)
  ("F" . describe-face)
  ("K" . describe-keymap)
  ("A" . apropos))

(al/bind-keys
  :map help-map
  :prefix-map al/info-map
  :prefix-doc "Map to display info manuals."
  :prefix-key "i"
  ("i" (info "dir"))
  ("c" (info "cl"))
  ("e" (info "elisp"))
  ("s" (info (al/src-dir-file "stumpwm/stumpwm.info")))
  ("o" (info "org"))
  ("g" (info "guile"))
  ("x" (info "guix"))
  ("M" (info "magit"))
  ("m" (info "make"))
  ("am" (info "automake"))
  ("ac" (info "autoconf"))
  ("t" (info "texinfo")))

;; Rebinding keys in `help-map' does not simply work: after evaluating
;; the code above, "C-h i" is still bound to `info'; resetting
;; `help-command' helps.
(fset 'help-command help-map)

(al/eval-after-load help-mode
  (al/bind-keys
   :map help-mode-map
   ("," . help-go-back)
   ("p" . help-go-forward))

  (al/call-at-hook help-mode-hook al/no-truncate-lines))

(al/bind-key "w" which-key-mode ctl-x-map)
(al/eval-after-load which-key
  (setq
   which-key-use-C-h-commands nil
   which-key-separator " "
   which-key-prefix-prefix ""
   which-key-idle-delay 0.8
   which-key-idle-secondary-delay 0.1
   which-key-add-column-padding 2
   which-key-max-display-columns 5))
(al/call-after-init which-key-mode)

(al/eval-settings-after-load
  (apropos (setq apropos-do-all t))
  (man "man")
  (woman "woman")
  (info "info")
  (texinfo "texinfo"))


;;; Spelling, translating

(al/bind-key "<XF86Spell>" tui/translate)

(al/bind-keys
 :prefix-map al/spell-map
 :prefix-doc "Map for flyspell and friends."
 :prefix-key "H-s"
 ("r" . flyspell-region)
 ("b" . flyspell-buffer)
 ("n" . flyspell-goto-next-error)
 ("H-n" . flyspell-goto-next-error))

(al/setq-no-warnings flyspell-use-meta-tab nil)

(al/eval-settings-after-load
  (ispell (ispell-change-dictionary "en" 'global))
  (flyspell "flyspell")
  (google-translate-core-ui "google-translate"))


;;; Darts, journal

(al/bind-keys
 :prefix-map al/darts-map
 :prefix-doc "Map for darts and journal."
 :prefix-key "M-D"
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
 ("t"   (al/find-file (al/journal-dir-file "tags"))))

(al/eval-after-load journal
  (al/load-settings "journal"))

(al/autoload "darts-value"
  darts-throw-string-to-points
  darts-throw-string-to-code)

(al/autoload "darts-daydata"
  darts-day-template
  darts-day-select)

(al/eval-after-load darts-daydata
  (setq
   darts-database "darts"
   darts-data-dir "~/darts/daytables"
   darts-exported-dir (expand-file-name "exported" darts-data-dir)
   darts-template-file (expand-file-name "template" darts-data-dir)))


;;; Initial scratch and message buffers

(setq
 initial-major-mode #'emacs-lisp-mode
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

(al/eval-at-hook messages-buffer-mode-hook
  (al/funcall 'hl-todo-mode)
  (setq buffer-read-only nil))

(al/call-after-init
  al/set-scratch-message
  al/reinit-messages-buffer)


;;; EMMS

(declare-function emms-playlist-simple-uniq "emms")
(declare-function al/emms-playlist-select "al-emms")

(al/bind-keys
  :prefix-map al/emms-map
  :prefix-doc "Map for EMMS."
  :prefix-key [ctrl-m]
  ([ctrl-m] . al/emms-switch-to-playlist-buffer)
  ("SPC" . emms-pause)
  ("M-SPC" . emms-stop)
  ("s" . al/emms-show)
  ("m" . emms-state-toggle-mode-line)
  ("n" . al/emms-notification-mode)
  ("B" . emms-browser)
  ("l"   (al/emms-playlist-select t))
  ("b" . al/emms-playlist-select)
  ("C-b" . al/emms-playlist-select)
  ("r" . emms-streams)
  ("g" . al/emms-seek-to)
  ("y" . al/emms-mpv-sync-playing-time)
  ("S" . al/emms-save-playlists)
  ("u"   (emms-playlist-simple-uniq)))

(al/bind-keys
  :map al/emms-map
  :prefix-map al/emms-play-map
  :prefix-doc "Map for playing EMMS entries."
  :prefix-key "p"
  ("t" . emms-play-directory-tree)
  ("d" . emms-play-directory)
  ("f" . emms-play-file)
  ("l" . emms-play-playlist)
  ("u" . emms-play-url))

(al/bind-keys
  :map al/emms-map
  :prefix-map al/emms-add-map
  :prefix-doc "Map for adding EMMS entries."
  :prefix-key "a"
  ("t" . emms-add-directory-tree)
  ("d" . emms-add-directory)
  ("f" . emms-add-file)
  ("l" . emms-add-playlist)
  ("u" . emms-add-url))

(al/setq-no-warnings
 emms-directory (al/emacs-data-dir-file "emms")
 emms-playlist-sort-prefix "s")

(al/eval-after-load emms
  (al/load-settings "emms"))


;;; Misc settings and packages

(al/call-at-hook (delete-frame-functions
                  kill-emacs-hook)
  al/save-everything)

(setq
 password-cache-expiry (* 24 60 60)
 line-number-display-limit-width 9999
 echo-keystrokes 0.2
 disabled-command-function nil
 inhibit-startup-screen t
 source-directory (al/src-dir-file "emacs"))

(setq
 warning-minimum-level :warning
 warning-suppress-types      ; do not pop up the *Warnings* buffer when:
 '(;; something long is executed in *shell*.
   (undo discard-info)))

(electric-indent-mode 0)

(setq use-short-answers t)

(al/bind-keys-from-vars 'special-mode-map 'al/lazy-moving-keys t)

(al/bind-keys
  :map process-menu-mode-map
  ("C-k" . process-menu-delete-process))

(al/eval-after-load al-process
  :load after-init
  (advice-add 'insert-directory :around #'al/call-with-locale)
  (al/process-hook-mode))

(al/eval-after-load server
  (setq
   server-kill-new-buffers nil
   server-temp-file-regexp
   (concat server-temp-file-regexp
           "\\|COMMIT_EDITMSG\\|git-rebase-todo")))

(al/eval-after-load al-server
  :load after-init
  (advice-add 'server-visit-files :around #'al/autoload-org-protocol)
  (when-let* ((name (al/server-name)))
    (setq al/server-running? t)
    (when (equal name "emms")
      (al/with-check
        :var '(al/mail-user-name   ; defined in "net.el"
               al/mail-user-name2) ; defined in "custom.el"
        (with-no-warnings
          (setq al/mail-user-name
                al/mail-user-name2)))
      (al/funcall 'al/save-place-mode)
      (al/funcall 'al/recentf-mode)
      (al/funcall 'appt-activate))))

;; Default value of `tramp-ssh-controlmaster-options' variable slows
;; down loading tramp significantly.  This should be set before tramp
;; is loaded.
(al/setq-no-warnings tramp-ssh-controlmaster-options "")

(al/eval-settings-after-load
  (tramp-sh
   (push 'tramp-own-remote-path tramp-remote-path)
   (push "LC_ALL=en_US.UTF-8" tramp-remote-process-environment)
   (push "DISPLAY=:0" tramp-remote-process-environment))
  (gnutls
   ;; http://comments.gmane.org/gmane.emacs.gnus.general/83413
   (setq gnutls-min-prime-bits nil))
  (calc (setq calc-angle-mode 'rad))
  (picture "picture")
  (artist "artist")
  (hexl "hexl")
  (diff-mode "diff-mode")
  (ediff "ediff")
  (view "view")
  (epa "epa")
  (sql "sql")
  (tabulated-list "tabulated-list")
  (bui "bui")
  (transient "transient"))

;;; settings.el ends here
