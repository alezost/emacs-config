;;; settings.el --- Miscellaneous settings

;; Copyright © 2012–2025 Alex Kost

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


;;; External processes

(with-eval-after-load 'al-process
  (defun al/set-zathura-theme (name)
    (make-symbolic-link name (al/config-dir-file "zathura/theme") t))

  (defun al/sync-zathura-theme (&rest args)
    "Synchronize zathura theme with the current emacs theme."
    (when (al/process-is-program args "zathura")
      (al/set-zathura-theme
       (format "%S-theme" (frame-parameter nil 'background-mode)))))

  (al/add-hook-maybe 'al/before-process-functions
    'al/sync-zathura-theme))
(al/add-after-init-hook 'al/enable-process-hooks)


;;; Minibuffer and completions

(al/bind-key* "M-t" execute-extended-command)

(setq
 completion-auto-select 'second-tab
 completions-sort 'historical
 completions-format 'one-column
 completions-max-height 20
 ;; completion-styles '(basic substring partial-completion)
 completion-styles '(flex)
 read-file-name-completion-ignore-case t
 read-buffer-completion-ignore-case t
 completion-ignore-case t
 enable-recursive-minibuffers t)

(al/add-hook-maybe 'minibuffer-setup-hook 'al/hbar-cursor-type)
(al/bind-keys-from-vars 'minibuffer-local-map 'al/minibuffer-keys)
(al/add-after-init-hook 'icomplete-vertical-mode)

(al/bind-keys
  :map minibuffer-local-completion-map
  ("SPC") ("?")
  ("RET" . icomplete-force-complete-and-exit))
(al/bind-keys
  :map minibuffer-local-must-match-map
  ("RET" . icomplete-force-complete-and-exit))

(al/bind-keys
  :map completion-list-mode-map
  ("." . previous-completion)
  ("e" . next-completion))

(with-eval-after-load 'icomplete
  (setq
   icomplete-scroll t
   icomplete-tidy-shadowed-file-names t
   icomplete-show-matches-on-no-input t)

  (defconst al/icomplete-keys
    ;; Don't bind "RET" in `icomplete-minibuffer-map' because it has a
    ;; priority over my `al/minibuffer-*-map' keymaps.
    ;; Use `minibuffer-local-completion-map' and
    ;; `minibuffer-local-must-match-map' above.
    '([remap minibuffer-complete-and-exit]
      ("TAB" . icomplete-force-complete)
      ("C-j" . exit-minibuffer)
      ("M-k" . al/minibuffer-copy-current-completion)
      ("C-." . icomplete-backward-completions)
      ("C-e" . icomplete-forward-completions))
    "Alist of auxiliary keys for icomplete maps.")
  (defconst al/icomplete-vertical-keys
    '(("H-a" . icomplete-vertical-goto-first)
      ("H-i" . icomplete-vertical-goto-last))
    "Alist of auxiliary keys for `icomplete-vertical-mode-minibuffer-map'.")
  (al/bind-keys-from-vars '(icomplete-minibuffer-map
                            icomplete-fido-mode-map)
    'al/icomplete-keys)
  (al/bind-keys-from-vars '(icomplete-vertical-mode-minibuffer-map)
    'al/icomplete-vertical-keys))

(with-eval-after-load 'al-minibuffer
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

  (advice-add 'completion--styles :override #'al/completion-styles)
  (advice-add 'completion-all-completions :around #'al/completion-all-completions)
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

(al/eval-after-init
  (require 'al-minibuffer nil t))


;;; Working with buffers: ibuffer, uniquify, …

(with-eval-after-load 'al-buffer
  (al/bind-keys
    :map al/switch-buffer-map
    ("M-b" . al/switch-to-other-buffer)
    ("M-B" . al/switch-to-prev-buffer)))

(al/bind-keys*
  ("M-b" . al/switch-to-previous-buffer)
  ("C-M-b" (find-file (al/notes-dir-file "bookmarks.org"))))

(al/bind-keys*
 :prefix-map al/buffer-map
 :prefix-docstring "Map for managing/switching to buffers."
 :prefix "C-b"
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
 ("w" . al/switch-to-w3m)
 ("m" . woman)
 ("k"   (kill-buffer nil))
 ("8" . al/switch-to-characters))

(with-eval-after-load 'uniquify
  (setq uniquify-buffer-name-style 'post-forward))

(with-eval-after-load 'ibuffer
  (setq ibuffer-default-sorting-mode 'filename/process)
  (defconst al/ibuffer-keys
    '(("u"   . ibuffer-visit-buffer)
      ("."   . ibuffer-backward-line)
      ("e"   . ibuffer-forward-line)
      ("M-." . ibuffer-backward-filter-group)
      ("M-e" . ibuffer-forward-filter-group)

      ("d"   . ibuffer-visit-buffer-other-window-noselect)
      ("C-d" . ibuffer-visit-buffer-other-window)
      ("C-l"   (ibuffer-update t))

      ("z"   . ibuffer-unmark-forward)
      ("Z"     (ibuffer-unmark-all ?\r))
      ("* o" . ibuffer-mark-old-buffers))
    "Alist of auxiliary keys for `ibuffer-mode-map'.")
  (al/bind-keys-from-vars 'ibuffer-mode-map 'al/ibuffer-keys)
  (al/add-hook-maybe 'ibuffer-mode-hook
    '(al/mode-ibuffer-info hl-line-mode)))


;;; Working with windows and frames

(setq split-width-threshold 120)

(al/add-hook-maybe 'window-configuration-change-hook
  'al/set-windows-num-property)

;; Open some buffers in the same window.
(setq
 same-window-buffer-names
 '("*Apropos"
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
   "*Process-Environment*"
   "*Process List*"
   "*Shadows*"
   "*YASnippet tables*")
 same-window-regexps
 '(".*\\.el\\.gz$"
   "shell\\*"))

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

(setq
 winner-dont-bind-my-keys t
 winner-ring-size 40)
(al/bind-keys
 ("<C-left>"  . winner-undo)
 ("<C-right>" . winner-redo))
(al/add-after-init-hook 'winner-mode)


;;; comint, shell, eshell

(setq shell-file-name "bash")

(al/bind-key "s-s" al/shell)
(al/bind-key "s-s" al/shells ctl-x-map)
(al/bind-keys*
 :prefix-map al/repl-map
 :prefix-docstring "Map for various REPLs."
 :prefix "C-n"
 ("C-s" . al/switch-to-shell-buffer)
 ("t"   . visit-ansi-term)
 ("e"   . eshell)
 ("i"   . ielm)
 ("s"   . al/sql-switch-or-connect)
 ("l"   . slime-repl)
 ("g"   . (lambda (arg) (interactive "P")
            (let (geiser-repl-use-other-window)
              (switch-to-guile arg))))
 ("G"   . al/geiser-socket-connect)
 ("h"   . al/haskell-interactive-switch-or-start)
 ("P"   . run-python)
 ("p"   . python-shell-switch-to-shell)
 ("L"   . lua-start-process)
 ("m"   . maxima)
 ("x"   . guix-switch-to-repl))

(with-eval-after-load 'comint
  (setq comint-move-point-for-output nil
        comint-buffer-maximum-size 5000
        comint-password-prompt-regexp
        (rx-to-string `(or (and bol "Password")
                           (regex ,comint-password-prompt-regexp))))
  (al/add-hook-maybe 'comint-output-filter-functions
    'comint-truncate-buffer)

  (defconst al/comint-keys
    '(("M-." . comint-previous-input)
      ("M-e" . comint-next-input)
      ("M->" . comint-previous-prompt)
      ("M-E" . comint-next-prompt)
      ("C-c c" . compilation-shell-minor-mode)
      ("C-c o" . al/comint-toggle-move-point)
      ("C-c C-d" (process-send-eof))
      ("TAB" . completion-at-point)
      ("RET" . al/comint-send-input-maybe)
      "C-d")
    "Alist of auxiliary keys for comint modes.")
  (al/bind-keys-from-vars 'comint-mode-map 'al/comint-keys))

(with-eval-after-load 'shell
  (defconst al/shell-keys
    '(("M-O" . shell-backward-command)
      ("M-U" . shell-forward-command))
    "Alist of auxiliary keys for `shell-mode-map'.")
  (al/bind-keys-from-vars 'shell-mode-map 'al/shell-keys t)
  (al/add-hook-maybe 'shell-mode-hook
    '(guix-build-log-minor-mode
      abbrev-mode
      al/no-truncate-lines)))

(with-eval-after-load 'al-shell
  (setq al/shell-buffer-alist
        (let ((dir al/download-dir))
          `(("*shell*" . ,dir)
            ("*shell*<2>" . ,dir)
            ("*shell*<3>" . ,dir)
            ("*shell-chess*" . ,(al/download-dir-file "chess"))
            ("*shell-math*" . ,(al/math-dir-file "video"))))))

(al/bind-keys
 ("C-z"   . eshell)
 ("C-M-z" . al/eshell-cd))

(with-eval-after-load 'eshell
  (setq eshell-directory-name (al/emacs-data-dir-file "eshell"))
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
      ("RET" . al/eshell-send-input-maybe)
      ("C-a" . eshell-bol)
      ("C-k" . al/eshell-kill-whole-line)
      ("M-." . eshell-previous-input)
      ("M-e" . eshell-next-input)
      ("M->" . eshell-previous-prompt)
      ("M-E" . eshell-next-prompt)
      ("M-r" . al/eshell-previous-matching-input-from-input)
      ("M-s" . al/eshell-next-matching-input-from-input))
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
  (when (require 'al-eshell nil t)
    (setq
     eshell-prompt-function 'al/eshell-prompt
     eshell-prompt-regexp al/eshell-prompt-regexp)
    (advice-add 'eshell/info :override 'al/eshell/info)))


;;; Button, custom, widget

(with-eval-after-load 'button
  (defconst al/button-map-keys
    '(("u" . push-button))
    "Alist of auxiliary keys for `button-map'.")
  (al/bind-keys-from-vars 'button-map 'al/button-map-keys t)
  (al/bind-keys-from-vars 'button-buffer-map 'al/button-keys t))

(with-eval-after-load 'wid-edit
  (defconst al/widget-button-keys
    '(("." . widget-backward)
      ("e" . widget-forward)
      ("u" . widget-button-press))
    "Alist of auxiliary keys for modes with widget buttons.")
  (defconst al/widget-field-keys
    '(("<M-tab>" . widget-complete)
      ("M-<" . widget-kill-line)
      ("<ctrl-i>" . widget-end-of-line)
      ("C-k"   (beginning-of-line) (widget-kill-line)))
    "Alist of auxiliary keys for modes with widget fields.")
  (al/bind-keys-from-vars 'widget-keymap 'al/widget-button-keys t)
  (al/bind-keys-from-vars 'widget-field-keymap 'al/widget-field-keys))

(with-eval-after-load 'cus-edit
  (al/bind-keys-from-vars 'custom-mode-map 'al/widget-button-keys t)
  (al/bind-keys
   :map custom-mode-map
   ("o" . Custom-goto-parent)
   ("g" . Custom-reset-standard)))


;;; Help, apropos, man, info

(setq apropos-do-all t)

(with-eval-after-load 'help
  (al/bind-keys
   :map help-map
   ("v" . al/describe-variable)
   ("s" . al/describe-symbol)
   ("x" . describe-syntax)
   ("F" . describe-face)
   ("A" . apropos))
  (al/bind-keys
   :map help-map
   :prefix-map al/info-map
   :prefix-docstring "Map to display info manuals."
   :prefix "i"
   ("i" (info "dir"))
   ("c" (info "cl"))
   ("e" (info "elisp"))
   ("s" (info (al/src-dir-file "stumpwm/stumpwm.info")))
   ("o" (info "org"))
   ("g" (info "guile"))
   ("x" (info "guix"))
   ("m" (info "make"))
   ("am" (info "automake"))
   ("ac" (info "autoconf"))
   ("t" (info "texinfo")))

  ;; Rebinding keys in `help-map' does not simply work: after evaluating
  ;; the code above, "C-h i" is still bound to `info'; resetting
  ;; `help-command' helps.
  (fset 'help-command help-map))

(with-eval-after-load 'help-mode
  (al/bind-keys
   :map help-mode-map
   ("," . help-go-back)
   ("p" . help-go-forward))
  (al/add-hook-maybe 'help-mode-hook 'al/no-truncate-lines))

(with-eval-after-load 'man
  (setq Man-notify-method 'pushy)
  (when (require 'al-file nil t)
    (setq Man-header-file-path
          (append (delq nil
                        (mapcar (lambda (p)
                                  (al/file-if-exists
                                   (expand-file-name "include" p)))
                                (al/guix-profiles)))
                  Man-header-file-path)))
  (when (require 'al-mode-line nil t)
    (al/mode-line-default-buffer-identification 'Man-mode))

  (defconst al/man-keys
    '(("M->" . Man-previous-section)
      ("M-E" . Man-next-section)
      ("h" . Man-previous-section)
      ("n" . Man-next-section)
      ("m" . Man-goto-section)
      ("g" . Man-update-manpage))
    "Alist of auxiliary keys for `Man-mode'.")
  (al/bind-keys-from-vars 'Man-mode-map
    '(al/button-keys al/man-keys)))

(with-eval-after-load 'woman
  (setq
   woman-fill-column (default-value 'fill-column)
   woman-default-indent 4)

  (defconst al/woman-keys
    '(("M-h" . WoMan-previous-manpage))
    "Alist of auxiliary keys for `woman-mode'.")
  (al/bind-keys-from-vars 'woman-mode-map 'al/woman-keys))

(with-eval-after-load 'info
  ;; `Info-additional-directory-list' is USELESS as it is appended to
  ;; `Info-directory-list' (by `Info-find-file' or by
  ;; `Info-insert-dir'), so the default manuals are searched first,
  ;; while I want my dirs to be searched first.
  (info-initialize)
  (setq Info-directory-list
        (append (al/existing-files
                 (al/emacs-my-packages-dir-file "guix/doc")
                 (al/devel-dir-file "guix/doc"))
                Info-directory-list))

  (al/bind-keys
   :map Info-mode-map
   ("." . Info-prev-reference)
   ("e" . Info-next-reference)
   ("c"   (Info-copy-current-node-name 0))
   ("o"   (Info-up) (goto-char (point-min)))
   ("O" . Info-top-node)
   ("u" . Info-follow-nearest-node)
   ("," . Info-history-back)
   ("p" . Info-history-forward)
   ("y" . Info-history)
   ("k" . Info-index-next)
   ("h" . Info-prev)
   ("n" . Info-next)
   ("H" . Info-help)))

(with-eval-after-load 'texinfo
  (require 'al-texinfo nil t)
  (defconst al/texinfo-keys
    '(("C-c c" . texinfo-insert-@code)
      ("C-c f" . texinfo-insert-@file)
      ("C-c i" . texinfo-insert-@item)
      ("C-c v" . texinfo-insert-@var)
      ("C-c M" . al/texinfo-insert-@menu)
      ("C-c E" . al/texinfo-insert-@example)
      ("C-c I" . al/texinfo-insert-@itemize)
      ("C-c T" . al/texinfo-insert-@table)
      ("C-c D" . al/texinfo-insert-@deffn))
    "Alist of auxiliary keys for `texinfo-mode'.")
  (al/bind-keys-from-vars 'texinfo-mode-map 'al/texinfo-keys))

(al/bind-key "w" which-key-mode ctl-x-map)
(with-eval-after-load 'which-key
  (setq
   which-key-use-C-h-commands nil
   which-key-separator " "
   which-key-prefix-prefix ""
   which-key-idle-delay 0.8
   which-key-idle-secondary-delay 0.1
   which-key-add-column-padding 2
   which-key-max-display-columns 5))
(al/add-after-init-hook 'which-key-mode)


;;; SQL

(with-eval-after-load 'sql
  (setq
   sql-product 'postgres
   sql-database "darts"
   sql-user user-login-name
   sql-connection-alist
   `((darts (sql-product 'postgres)
            (sql-server "")
            (sql-database "darts")
            (sql-user ,user-login-name))
     (ptmp  (sql-product 'postgres)
            (sql-server "")
            (sql-database "tmp")
            (sql-user ,user-login-name))
     (mtmp  (sql-product 'mariadb)
            (sql-server "")
            (sql-database "tmp")
            (sql-user ,user-login-name))))

  (defconst al/sql-keys
    '(("C-v"   . sql-send-region)
      ("C-M-v" . sql-send-paragraph)
      ("M-s-v" . sql-send-buffer)
      ("C-c C-z" . al/sql-switch-to-repl))
    "Alist of auxiliary keys for `sql-mode'.")
  (al/bind-keys-from-vars 'sql-mode-map 'al/sql-keys)

  ;; I just can't stand the default key bindings.
  (al/clean-map 'sql-interactive-mode-map)
  (set-keymap-parent sql-interactive-mode-map comint-mode-map)

  (when (require 'al-sql nil t)
    (advice-add 'sql-highlight-product
      :override 'al/sql-highlight-product)
    (al/add-hook-maybe 'sql-mode-hook
      'al/sql-set-comment-start-skip)
    (al/add-hook-maybe 'sql-interactive-mode-hook
      '(al/sql-save-history
        al/sql-highlight-product
        al/sql-completion-setup)))

  ;; Fix bug with mariadb prompt:
  ;; <http://debbugs.gnu.org/cgi/bugreport.cgi?bug=17426>.
  (sql-set-product-feature 'mysql :prompt-regexp
                           "^\\(?:mysql\\|mariadb\\).*> "))

(with-eval-after-load 'mysql
  (setq mysql-user sql-user)
  (when (require 'al-mysql nil t)
    (advice-add 'mysql-shell-query
      :override 'al/mysql-shell-query)))

(with-eval-after-load 'sql-completion
  (setq
   sql-mysql-database sql-database
   sql-mysql-exclude-databases
   '("mysql" "information_schema" "performance_schema"))
  (require 'cl nil t))

(with-eval-after-load 'al-sql
  (setq al/sql-history-dir (al/emacs-data-dir-file "sql")))


;;; Darts, journal

(al/bind-keys
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
 ("t"   (find-file (al/journal-dir-file "tags"))))

(with-eval-after-load 'journal
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

(al/autoload "darts-value"
  darts-throw-string-to-points
  darts-throw-string-to-code)

(al/autoload "darts-daydata"
  darts-day-template
  darts-day-select)

(with-eval-after-load 'darts-daydata
  :config
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

(al/eval-after-init
  (al/set-scratch-message)
  (al/reinit-messages-buffer))

(al/add-hook-maybe 'messages-buffer-mode-hook
  (list 'hl-todo-mode
        (lambda () (setq buffer-read-only nil))))


;;; Misc settings and packages

(setq
 password-cache-expiry (* 24 60 60)
 line-number-display-limit-width 9999
 echo-keystrokes 0.2
 disabled-command-function nil
 inhibit-startup-screen t
 find-function-C-source-directory (al/src-dir-file "emacs-git/src"))

(setq
 ;; `bibtex-completion-get-entry1' uses :warning as a TYPE instead of
 ;; LEVEL (TODO this should be reported).  So it cannot be suppressed;
 ;; that's why I just ignore all the warnings.
 warning-minimum-level :error
 warning-suppress-types      ; do not pop up the *Warnings* buffer when:
 '(;; something long is executed in *shell*.
   (undo discard-info)))

(electric-indent-mode 0)

(advice-add 'yes-or-no-p :override 'y-or-n-p)

(al/bind-keys-from-vars 'special-mode-map 'al/lazy-moving-keys t)

(with-eval-after-load 'server
  (setq
   server-kill-new-buffers nil
   server-temp-file-regexp
   (concat server-temp-file-regexp
           "\\|COMMIT_EDITMSG\\|git-rebase-todo")))

;; Default value of `tramp-ssh-controlmaster-options' variable slows
;; down loading tramp significantly.  This should be set before tramp
;; was loaded.
(setq tramp-ssh-controlmaster-options "")

(with-eval-after-load 'tramp-sh
  (push 'tramp-own-remote-path tramp-remote-path)
  (push "LC_ALL=en_US.UTF-8" tramp-remote-process-environment)
  (push "DISPLAY=:0" tramp-remote-process-environment))

(with-eval-after-load 'gnutls
  ;; http://comments.gmane.org/gmane.emacs.gnus.general/83413
  (setq gnutls-min-prime-bits nil))

(with-eval-after-load 'calc
  (setq calc-angle-mode 'rad))

(with-eval-after-load 'picture
  (defconst al/picture-keys
    '(("M-O" . picture-movement-left)
      ("M-U" . picture-movement-right)
      ("M->" . picture-movement-up)
      ("M-E" . picture-movement-down)
      ("M-<" . picture-movement-nw)
      ("M-P" . picture-movement-ne)
      ("M-Q" . picture-movement-sw)
      ("M-K" . picture-movement-se))
    "Alist of auxiliary keys for `picture-mode-map'.")
  (al/bind-keys-from-vars 'picture-mode-map 'al/picture-keys))

(with-eval-after-load 'artist
  (defconst al/artist-keys
    '(("C-o" . artist-backward-char)
      ("C-u" . artist-forward-char)
      ("C-." . artist-previous-line)
      ("C-e" . artist-next-line))
    "Alist of auxiliary keys for `artist-mode-map'.")
  (al/bind-keys-from-vars 'artist-mode-map 'al/artist-keys))

(with-eval-after-load 'hexl
  (al/bind-keys
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

(with-eval-after-load 'diff-mode
  (defconst al/diff-shared-keys
    '(("." . diff-hunk-prev)
      (">" . diff-file-prev)
      ("e" . diff-hunk-next)
      ("E" . diff-file-next))
    "Alist of auxiliary keys for `diff-mode-shared-map'.")
  (defconst al/diff-keys
    '(("H-u" . diff-undo)
      ("M-." . diff-hunk-prev)
      ("M->" . diff-file-prev)
      ("M-e" . diff-hunk-next)
      ("M-E" . diff-file-next))
    "Alist of auxiliary keys for `diff-mode-map'.")
  (al/bind-keys-from-vars 'diff-mode-shared-map 'al/diff-shared-keys t)
  (al/bind-keys-from-vars 'diff-mode-map 'al/diff-keys))

(with-eval-after-load 'ediff
  (when (require 'al-ediff nil t)
    (al/add-hook-maybe 'ediff-before-setup-hook
      'al/ediff-save-window-configuration)
    (al/add-hook-maybe 'ediff-quit-hook
      'al/ediff-restore-window-configuration
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

(with-eval-after-load 'view
  (defconst al/view-keys
    '(("v" . View-exit))
    "Alist of auxiliary keys for `view-mode-map'.")
  (al/bind-keys-from-vars 'view-mode-map
    '(al/lazy-moving-keys al/view-keys)
    t))

(with-eval-after-load 'epa
  (require 'wid-edit) ; for `al/widget-button-keys' (it is required anyway)
  (al/bind-keys-from-vars 'epa-key-list-mode-map
    'al/widget-button-keys t)
  (al/bind-keys
   :map epa-key-list-mode-map
   ("z" . epa-unmark-key)))

(al/bind-keys
 :prefix-map al/tags-map
 :prefix-docstring "Map for tags."
 :prefix "M-T"
 ("M-T" . find-tag)
 ("d"     (find-tag (find-tag-default)))
 ("r"   . find-tag-regexp)
 ("n"   . tags-loop-continue)
 ("v"   . visit-tags-table)
 ("c"   . al/create-tags))

(with-eval-after-load 'tabulated-list
  (defconst al/tabulated-list-keys
    '(("s" . tabulated-list-sort))
    "Alist of auxiliary keys for `tabulated-list-mode-map'.")
  (al/bind-keys-from-vars 'tabulated-list-mode-map
    '(al/lazy-moving-keys al/tabulated-list-keys)
    t)
  (add-hook 'tabulated-list-mode-hook 'hl-line-mode))

(with-eval-after-load 'simple
  (defconst al/process-menu-mode-keys
    '(("C-k" . process-menu-delete-process))
    "Alist of auxiliary keys for `process-menu-mode-map'.")
  (al/bind-keys-from-vars 'process-menu-mode-map
    'al/process-menu-mode-keys))

(with-eval-after-load 'bui
  (defconst al/bui-keys
    '(("," . bui-history-back)
      ("p" . bui-history-forward))
    "Alist of auxiliary keys for `bui-map'.")
  (al/bind-keys-from-vars 'bui-map 'al/bui-keys))

(with-eval-after-load 'bui-list
  (defconst al/bui-list-keys
    '(("u" . bui-list-describe)
      ("z" . bui-list-unmark)
      ("Z" . bui-list-unmark-all))
    "Alist of auxiliary keys for `bui-list-mode-map'.")
  (al/bind-keys-from-vars 'bui-list-mode-map 'al/bui-list-keys))

(with-eval-after-load 'transient
  (setq
   transient--buffer-name "*transient*"
   transient-detect-key-conflicts t
   ;; transient-highlight-mismatched-keys t
   ;; transient--debug t
   transient-enable-popup-navigation t
   transient-read-with-initial-input nil
   transient-mode-line-format mode-line-format
   transient-display-buffer-action '(display-buffer-below-selected))

  (defconst al/transient-base-keys
    '("C-v" "M-v"
      ("C-g" . transient-quit-all)
      ("C-q" . transient-quit-one)
      ("DEL" . transient-quit-one))
    "Alist of auxiliary keys for `transient-base-map'.")
  (al/bind-keys-from-vars 'transient-base-map
    'al/transient-base-keys)

  (defconst al/transient-keys
    '("C-h")
    "Alist of auxiliary keys for `transient-map'.")
  (al/bind-keys-from-vars 'transient-map 'al/transient-keys)

  (defconst al/transient-navigation-keys
    '(("<tab>" . transient-forward-button)
      ("<backtab>" . transient-backward-button))
    "Alist of auxiliary keys for `transient-popup-navigation-map'.")
  (al/bind-keys-from-vars 'transient-popup-navigation-map
    'al/transient-navigation-keys)

  (transient-suffix-put 'transient-common-commands
                        "C-g" :command 'transient-quit-all)
  (transient-suffix-put 'transient-common-commands
                        "C-q" :command 'transient-quit-one)

  (when (require 'al-transient nil t)
    (advice-add 'transient--minibuffer-setup :override #'ignore)
    (advice-add 'transient--minibuffer-exit :override #'ignore)
    (advice-add 'transient--push-keymap :override #'al/transient-push-keymap)
    (advice-add 'transient--pop-keymap :override #'al/transient-pop-keymap)
    (advice-add 'transient--pre-command :around #'al/transient-fix-pre/post-command)
    (advice-add 'transient--post-command :around #'al/transient-fix-pre/post-command)
    (advice-add 'transient--delete-window :around #'al/transient-fix-delete-window)
    (advice-add 'transient--init-transient :after #'al/transient-fix-init)
    (advice-add 'transient--show :after #'al/transient-fix-show)))

;;; settings.el ends here
