;;; prog.el --- Programming modes and tools  -*- lexical-binding: t -*-

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


;;; Working with elisp: eldoc, edebug, debugger, …

(setq
 eval-expression-print-length nil
 parens-require-spaces nil)

(al/bind-key* "M-v" al/pp-eval-expression)
(al/bind-keys
 ("C-v"   . al/eval-dwim)
 ("C-s-v" . al/pp-eval-dwim)
 ("C-S-v" . al/pp-macroexpand-last-sexp)
 ("C-M-v" . eval-defun)
 ("M-s-v" . eval-buffer)
 ("C-d"   . elisp-slime-nav-describe-elisp-thing-at-point)
 ("M-d"   . elisp-slime-nav-find-elisp-thing-at-point))
(al/bind-keys
 :prefix-map al/doc-map
 :prefix-doc "Map for documentation/finding definitions."
 :prefix-key "C-M-d"
 ("f" . find-function)
 ("v" . find-variable)
 ("F" . find-face-definition)
 ("b" . describe-personal-keybindings))

(al/eval-after-load lisp-mode
  (defconst al/lisp-shared-keys
    '(("<C-M-tab>" . al/indent-sexp))
    "Alist of auxiliary keys for `lisp-mode-shared-map'.")
  (al/bind-keys-from-vars 'lisp-mode-shared-map 'al/lisp-shared-keys)
  (al/bind-keys-from-vars 'lisp-mode-map)

  (al/modify-page-break-syntax lisp-mode-syntax-table)

  ;; `lisp-mode' package is already loaded on Emacs start, and I don't
  ;; want to load additional Common Lisp functionality on start.  So
  ;; instead of requiring `al-clisp' here, it is loaded on the first run
  ;; of `lisp-mode' major mode (by `lisp-mode-hook').
  (al/eval-at-hook lisp-mode-hook
    :once t
    (al/require al-clisp)
    ;; Update fontification of the current buffer.
    (lisp-mode)))

(al/eval-after-load al-clisp
  (al/clisp-add-font-lock-keywords))

(al/eval-after-load elisp-mode
  (defconst al/elisp-keys
    '(("C-c C-z" . al/ielm-other-window))
    "Alist of auxiliary keys for `emacs-lisp-mode-map'.")
  (al/bind-keys-from-vars
      '(emacs-lisp-mode-map
        lisp-interaction-mode-map)
    'al/elisp-keys)

  (al/require al-elisp))

(al/eval-after-load al-elisp
  (al/elisp-add-font-lock-keywords)
  (advice-add 'elisp--form-quoted-p :override #'al/elisp-form-quoted-p))

(al/eval-after-load ielm
  (setq ielm-prompt "EL> ")
  (defconst al/ielm-keys
    '("C-j"
      ("RET" . ielm-send-input))
    "Alist of auxiliary keys for `ielm-map'.")
  (al/bind-keys-from-vars 'ielm-map
    '(al/lisp-shared-keys al/comint-keys al/ielm-keys))

  (al/call-at-hook ielm-mode-hook al/no-truncate-lines))

(al/eval-after-load eldoc
  (setq eldoc-idle-delay 0.3))

(al/eval-after-load edebug
  (al/bind-keys
   :map edebug-mode-map
   ("v"   . edebug-eval-expression)
   ("C-v" . edebug-eval-last-sexp)))

(al/bind-key "C-c d" toggle-debug-on-error)
(al/eval-after-load debug
  (al/bind-keys-from-vars 'debugger-mode-map 'al/button-keys t)
  (al/bind-keys
   :map debugger-mode-map
   ("v" . debugger-eval-expression)
   ("l" . debugger-toggle-locals)
   ("f" . debugger-list-functions)))

(al/eval-after-load ert
  (defconst al/ert-results-keys
    '(("RET" . ert-results-describe-test-at-point)
      ("g" . ert-results-rerun-all-tests)
      ("h" . ert-results-previous-test))
    "Alist of auxiliary keys for `ert-results-mode-map'.")
  (al/bind-keys-from-vars 'ert-results-mode-map
    '(al/button-keys al/ert-results-keys)))

(al/eval-after-load pp
  (al/require al-pp))

(al/eval-after-load al-pp
  (advice-add 'pp-display-expression :after #'al/pp-enable-undo))


;;; SLY

(al/setq-no-warnings
 sly-contribs
 '(sly-mrepl
   sly-autodoc
   sly-fancy-inspector
   sly-fancy-trace
   sly-scratch
   sly-package-fu
   sly-trace-dialog
   sly-stickers
   sly-indentation
   sly-tramp))

;; `al/sly-keys' is used by `al/erc-channel-config'.
(defconst al/sly-keys
  '(("C-c"     . sly-prefix-map)
    ("C-v"     . al/sly-eval-dwim)
    ("C-M-v"   . sly-eval-defun)
    ("M-s-v"   . sly-eval-buffer)
    ("C-S-v"   . sly-macroexpand-all)
    ("C-d"     . sly-describe-symbol)
    ("M-d"     . sly-edit-definition)
    ("C-M-d"   . sly-doc-map)))

(al/eval-after-load sly
  (setq inferior-lisp-program "sbcl")

  (defconst al/sly-prefix-keys
    '("C-d"
      ("C-z"   (al/sly 'other-window))
      ("C-j" . al/sly-switch-to-repl-and-enter)))

  (defconst al/sly-xref-keys
    '(("." . sly-xref-prev-line)
      ("e" . sly-xref-next-line)
      ("u" . sly-xref-goto)
      ("d" . sly-xref-show)))
  (al/bind-keys-from-vars 'sly-xref-mode-map 'al/sly-xref-keys)

  (defconst al/sly-db-keys
    '(("."   . sly-db-up)
      ("e"   . sly-db-down)
      (">"   . sly-db-details-up)
      ("E"   . sly-db-details-down)
      ("M-." . sly-db-beginning-of-backtrace)
      ("M-e" . sly-db-end-of-backtrace)))
  (al/bind-keys-from-vars 'sly-db-mode-map 'al/sly-db-keys)

  (defconst al/sly-db-frame-keys
    '(("TAB" . sly-db-toggle-details)
      ("d"   . sly-db-show-frame-source)
      ("v"   . sly-db-eval-in-frame)))
  (al/bind-keys-from-vars 'sly-db-frame-map
    '(al/sly-db-frame-keys al/button-keys))

  (defconst al/sly-doc-keys
    '(("C-d" . sly-documentation-lookup)))
  (al/bind-keys-from-vars 'sly-doc-map 'al/sly-doc-keys)

  ;; `sly-editing-mode' is a useless wrapper for `sly-mode' but some
  ;; contrib modules add commands to its hook.  So making only
  ;; `sly-mode' work instead of `sly-editing-mode' would require too
  ;; much configuration.  At least, clean its keymap.
  (al/clean-map 'sly-editing-mode-map)

  ;; Clearly, contrib packages should be configured ONCE after loading
  ;; sly.  Instead, `sly--setup-contribs' is called on EVERY connection
  ;; (by `sly-setup-connection') to give you double benefit: no contrib
  ;; modules before the first REPL start and useless reevaluating of the
  ;; same code multiple times.
  (sly--setup-contribs)
  (advice-add 'sly--setup-contribs :override #'ignore)

  ;; Bind `sly-mode' keys after loading contribs because `sly-mrepl'
  ;; binds "C-c C-z".
  (al/bind-keys-from-vars 'sly-prefix-map 'al/sly-prefix-keys)
  (al/bind-keys-from-vars 'sly-mode-map 'al/sly-keys)

  ;; Fix some indentation broken by `sly-cl-indent'.
  (when (al/require al-clisp)
    (al/clisp-setup-indentation))

  (al/require al-sly))

(al/eval-after-load al-sly
  (setq sly--mode-line-format `(:eval (al/sly-mode-line-format)))
  (advice-add 'sly-make-action-button
    :around #'al/sly-change-action-button-label))

(al/eval-after-load sly-mrepl
  (defconst al/sly-repl-keys
    '("TAB"
      ("C-c C-d" . al/sly-repl-disconnect-or-quit)
      ("M-r" . comint-history-isearch-backward-regexp)
      ("M-." . sly-mrepl-previous-input-or-button)
      ("M-e" . sly-mrepl-next-input-or-button)
      ("M->" . sly-mrepl-previous-prompt)
      ("M-E" . sly-mrepl-next-prompt)))
  (al/bind-keys-from-vars 'sly-mrepl-mode-map 'al/sly-repl-keys))

(al/eval-after-load sly-autodoc
  (al/clean-map 'sly-autodoc-mode-map))


;;; Scheme, geiser

(al/eval-after-load scheme
  (put 'plist-new 'scheme-indent-function 1)
  (al/modify-page-break-syntax scheme-mode-syntax-table)

  (al/call-at-hook scheme-mode-hook guix-devel-mode)

  (al/require al-scheme))

(al/eval-after-load al-scheme
  (al/scheme-add-font-lock-keywords)
  (al/call-at-hook scheme-mode-hook
    al/scheme-fix-docstring-font-lock
    al/scheme-fix-fill)
  (advice-add 'scheme-indent-function
    :override 'al/scheme-indent-function))

(al/eval-after-load xscheme
  ;; I don't how this `xscheme' package is loaded from time to time but
  ;; it pollutes `scheme-mode-map' (in particular, it breaks my "M-o"
  ;; key binding).
  (al/clean-map 'scheme-mode-map))

(defconst al/geiser-keys
  '(("C-v"   . al/geiser-eval-dwim)
    ("C-S-v" . geiser-expand-last-sexp)
    ("C-M-v" . geiser-eval-definition)
    ("M-s-v" . geiser-eval-buffer)
    ("C-d"   . geiser-doc-symbol-at-point)
    ("M-d"   . geiser-edit-symbol-at-point)
    ("C-M-d" . al/geiser-doc-map)
    ("C-c l" . al/geiser-add-to-load-path)
    ("C-c a" . geiser-autodoc-mode)
    ("C-c j" . switch-to-geiser-module)
    ;; Although this "C-c C-z" exists in `geiser-mode-map',
    ;; `al/geiser-keys' is also used in ERC buffers.
    ("C-c C-z" . geiser-mode-switch-to-repl)
    ("C-c C-j" . geiser-mode-switch-to-repl-and-enter)))

(al/eval-after-load geiser-mode
  (defvar al/geiser-doc-map (make-sparse-keymap))
  (al/bind-keys
   :map al/geiser-doc-map
   ("d" . geiser-doc-symbol-at-point)
   ("i" . geiser-doc-look-up-manual)
   ("m" . geiser-doc-module)
   ("s" . geiser-autodoc-show)
   ("t" . geiser-autodoc-mode))
  (al/bind-keys-from-vars 'geiser-mode-map 'al/geiser-keys)

  ;; `geiser-mode' requires `geiser-repl', not vice versa.  So if keys
  ;; are bound after loading `geiser-repl', "C-M-d" in REPL is bound to
  ;; a non-existing `al/geiser-doc-map' command (because
  ;; `al/geiser-doc-map' variable with keymap does not exist yet).
  (defconst al/geiser-repl-keys
    '("TAB" "C-c k"
      ("RET" . al/geiser-repl-enter-dwim)
      ("C-k" . al/geiser-repl-kill-whole-line)
      ("C-a" . geiser-repl--bol)
      ("C-c C-d" . geiser-repl-exit)))
  (al/bind-keys-from-vars 'geiser-repl-mode-map
    '(al/comint-keys al/geiser-keys al/geiser-repl-keys)))

(al/eval-after-load geiser-repl
  (setq
   geiser-repl-skip-version-check-p t
   geiser-repl-use-other-window t
   geiser-repl-history-filename (al/emacs-data-dir-file "geiser-history"))

  (al/call-at-hook geiser-repl-mode-hook
    al/inhibit-field-motion
    al/no-syntactic-font-lock)

  (al/require al-geiser))

(al/eval-after-load geiser-impl
  (setq-default geiser-scheme-implementation 'guile)
  (setq geiser-active-implementations '(guile racket))
  (geiser-implementation-extension 'racket "rkt[dl]?"))

(al/eval-after-load geiser-doc
  (defconst al/geiser-doc-keys
    '((","   . geiser-doc-previous)
      ("p"   . geiser-doc-next)
      ("C-d" . al/geiser-doc-doc-symbol-at-point)
      ("M-d" . geiser-doc-edit-symbol-at-point))
    "Alist of auxiliary keys for `geiser-doc-mode'.")
  (al/bind-keys-from-vars 'geiser-doc-mode-map
    '(al/button-keys al/geiser-keys al/geiser-doc-keys)))

(al/eval-after-load geiser-debug
  (advice-add 'geiser-debug-mode :override 'scheme-mode))

(al/eval-after-load al-geiser
  (setq
   geiser-repl-buffer-name-function #'al/geiser-repl-buffer-name
   al/geiser-sockets
   (let* ((xdg-dir (getenv "XDG_RUNTIME_DIR"))
          (dir (expand-file-name "guile-daemon"
                                 (or xdg-dir
                                     (getenv "XDG_CONFIG_HOME")
                                     "~/.config")))
          (dir (if xdg-dir
                   dir
                 (expand-file-name "run" dir))))
     (list (expand-file-name "socket" dir)
           ;; "herd start repl" should be run before using this socket.
           (expand-file-name "shepherd/repl" xdg-dir)))))


;;; Haskell

(defconst al/haskell-general-keys
  '(("M-d" . haskell-mode-jump-to-def-or-tag))
  "Alist of auxiliary keys for Haskell modes.")

(al/eval-after-load haskell-mode
  (defconst al/haskell-keys
    '(("C-c C-z" . haskell-interactive-switch))
    "Alist of auxiliary keys `haskell-mode-map'.")
  (al/bind-keys-from-vars 'haskell-mode-map
    '(al/haskell-general-keys al/haskell-keys)))

(al/eval-after-load haskell-interactive-mode
  (defconst al/haskell-interactive-keys
    '(("M-." . haskell-interactive-mode-history-previous)
      ("M-e" . haskell-interactive-mode-history-next)
      ("M->" . haskell-interactive-mode-prompt-previous)
      ("M-E" . haskell-interactive-mode-prompt-next)
      ("C-a" . haskell-interactive-mode-beginning)
      ("C-k" . haskell-interactive-mode-kill-whole-line)
      ("C-c C-d" (haskell-session-kill 'leave-buffer)))
    "Alist of auxiliary keys for `haskell-interactive-mode'.")
  (al/bind-keys-from-vars 'haskell-interactive-mode-map
    '(al/haskell-general-keys al/haskell-interactive-keys)))


;;; GDB, GUD

(al/setq-no-warnings gud-key-prefix (kbd "M-G"))

(al/eval-after-load gud
  ;; GUD binds its keys inside `gdb' and `gud-gdb' commands.
  (al/call-at-hook (gdb-mode-hook
                    gud-gdb-mode-hook)
    (al/bind-keys-from-vars 'gud-mode-map 'al/comint-keys)))


;;; Compilation, Makefile

(al/eval-after-load make-mode
  (defconst al/make-keys
    '(("M->" . makefile-previous-dependency)
      ("M-E" . makefile-next-dependency))
    "Alist of auxiliary keys for `make-mode-map'.")
  (al/bind-keys-from-vars 'makefile-mode-map 'al/make-keys))

(al/eval-after-load compile
  (setq
   ;; Don't ask, don't save.
   compilation-ask-about-save nil
   compilation-save-buffers-predicate 'ignore)

  (defconst al/compilation-common-keys
    '(("C-M-h" . compilation-previous-error)
      ("C-M-n" . compilation-next-error)
      ("C-M-." . compilation-previous-error)
      ("C-M-e" . compilation-next-error))
    "Alist of auxiliary keys that should be bound in any compilation mode.")
  (defconst al/compilation-keys
    '(("."   . compilation-previous-error)
      ("e"   . compilation-next-error)
      ("M-." . previous-error-no-select)
      ("M-e" . next-error-no-select))
    "Alist of auxiliary keys for compilation modes.")
  (defconst al/compilation-button-keys
    '(("u"   . compile-goto-error))
    "Alist of auxiliary keys for `compilation-button-map'.")
  (al/bind-keys-from-vars 'compilation-button-map
    'al/compilation-button-keys)
  (al/bind-keys-from-vars 'compilation-shell-minor-mode-map
    'al/compilation-common-keys)
  (al/bind-keys-from-vars
      '(compilation-mode-map compilation-minor-mode-map)
    '(al/compilation-common-keys al/compilation-keys))

  (al/call-at-hook compilation-mode-hook al/hl-line-mode)

  (al/require al-compilation))

(al/eval-after-load al-compilation
  (al/setq-file
   al/compilation-sound-success (al/sound-dir-file "bell.oga")
   al/compilation-sound-error   (al/sound-dir-file "splat.wav"))

  (add-hook 'compilation-finish-functions 'al/compilation-notify))


;;; Version control

(al/setq-no-warnings
 magit-auto-revert-mode nil
 magit-define-global-key-bindings nil)

(al/eval-after-load vc-hooks
  (setq
   vc-make-backup-files t
   vc-handled-backends nil))

;; I don't load "magit-autoloads.el", so autoload some commands.
(al/autoload "magit"
  magit-dispatch)
(al/autoload "magit-status"
  magit-status)
(al/autoload "magit-blame"
  magit-blame)
(al/autoload "magit-log"
  magit-log-current)

;; By default, when `with-editor' library is loaded, it runs
;; "<emacsclient> --version" shell command HUNDREDS of times (for any
;; possible name of <emacsclient> executable in all dirs from PATH).
;; This happens during initializing `with-editor-emacsclient-executable'
;; variable (when `with-editor-locate-emacsclient' is called).
(al/setq-no-warnings with-editor-emacsclient-executable nil)
(al/eval-after-load with-editor
  (setq with-editor-emacsclient-executable
        (expand-file-name "emacsclient" invocation-directory)))

(al/eval-after-load al-magit
  (al/bind-keys
    :map al/magit-switch-map
    ("M-m" . al/magit-switch-buffer)))

(al/bind-keys
 :prefix-map al/magit-map
 :prefix-doc "Map for magit and git stuff."
 :prefix-key "M-m"
 ("M-m" . al/magit-switch-buffer)
 ("b"   (al/magit-switch-buffer 'all))
 ("B" . magit-blame)
 ("c" . al/magit-show-commit)
 ("d" . magit-dispatch)
 ("s" . magit-status)
 ("l" . magit-log-current)
 ("k" . al/browse-at-remote-kill)
 ("u" . browse-at-remote))

(defconst al/magit-common-keys
  '(("v"   . magit-git-command)
    "M-m")
  "Alist of auxiliary keys that should be bound in any magit mode.")
(defconst al/magit-history-keys
  '((","   . magit-go-backward)
    ("p"   . magit-go-forward))
  "Alist of auxiliary keys for moving by magit history.")
(defconst al/magit-scroll-diff-keys
  '(("SPC" . magit-diff-show-or-scroll-up)
    ("DEL" . magit-diff-show-or-scroll-down))
  "Alist of auxiliary keys for scrolling magit diff in other window.")
(defconst al/magit-moving-keys
  '((">"   . magit-section-up)
    ("."   . magit-section-backward)
    ("e"   . magit-section-forward)
    ("M-." . magit-section-backward-sibling)
    ("M-e" . magit-section-forward-sibling))
  "Alist of auxiliary keys for moving by magit sections.")

(al/eval-after-load magit-status
  (setq magit-status-initial-section '(((unstaged) (status)) 1)))

(al/eval-after-load magit-section
  (setq
   ;; I don't use global line numbers modes anyway, so there is no need
   ;; in additional checks.
   magit-section-disable-line-numbers nil
   magit-section-initial-visibility-alist
   '((untracked . show)
     (unstaged . show)
     (unpushed . show)
     (stashes . show))))

(al/eval-after-load magit-branch
  (setq magit-branch-read-upstream-first nil)

  (transient-suffix-put 'magit-branch 'magit-branch-rename :key "R")
  (transient-suffix-put 'magit-branch 'magit-pull.rebase :key "U")
  )

(al/eval-after-load magit-merge
  (oset (get 'magit-merge 'transient--prefix)
        value '("--ff-only")))

(al/eval-after-load magit-tag
  (transient-suffix-put 'magit-tag 'magit-tag-create :key "n"))

(al/eval-after-load magit-mode
  (setq
   magit-bury-buffer-function #'ignore
   magit-save-repository-buffers nil
   magit-uniquify-buffer-names nil)

  (defconst al/magit-keys
    '(("<backtab>" . magit-section-cycle-global)
      ("H-SPC" . magit-diff-show-or-scroll-up)
      ("M-k" . magit-copy-section-value)
      ("u" . magit-show-commit)
      ("U" . magit-unstage)
      ("E" . magit-ediff-dwim)
      ("C" . magit-cherry-pick)
      ("R" . magit-remote)
      ("1" . magit-section-show-level-1-all)
      ("2" . magit-section-show-level-2-all)
      ("3" . magit-section-show-level-3-all)
      ("4" . magit-section-show-level-4-all)
      "M-1" "M-2" "M-3" "M-4")
    "Alist of auxiliary keys for `magit-mode-map'.")
  (al/bind-keys-from-vars 'magit-mode-map
    '(al/lazy-scrolling-keys
      al/magit-common-keys
      al/magit-moving-keys
      al/magit-keys))

  (al/require al-magit))

(al/eval-after-load magit-popup
  (setq
   magit-popup-display-buffer-action '((display-buffer-at-bottom))
   magit-popup-show-common-commands nil
   magit-popup-use-prefix-argument 'default)

  (defconst al/magit-popup-keys
    '(("DEL" . al/magit-popup-previous-or-quit)
      ("M-." . backward-button)
      ("M-e" . forward-button)
      ("M-h" . magit-popup-toggle-show-common-commands))
    "Alist of auxiliary keys for `magit-popup-mode-map'.")
  (al/bind-keys-from-vars 'magit-popup-mode-map
    'al/magit-popup-keys
    t)

  (al/call-at-hook magit-popup-mode-hook al/bar-cursor-type)

  ;; Move away from buttons.  Adding `al/beginning-of-buffer' to
  ;; `magit-popup-mode-hook' wouldn't work because
  ;; `magit-refresh-popup-buffer' is called after the mode is set.
  (advice-add 'magit-refresh-popup-buffer
    :after 'al/beginning-of-buffer))

;; `magit-log-margin' should be set before magit is loaded, as
;; the other margins are defined from this one.
(setq magit-log-margin '(t age-abbreviated magit-log-margin-width t 20))

(al/eval-after-load magit-log
  (put 'magit-log-mode 'magit-log-default-arguments
       '("-n99" "--decorate"))

  (transient-suffix-put 'magit-log 'magit-log:--grep :key "=g") ; grep
  (transient-suffix-put 'magit-log 'magit-log:-G :key "=p")     ; patch
  (transient-suffix-put 'magit-log 'magit:-- :key "=f")         ; file

  (defconst al/magit-log-select-keys
    '(("m" . magit-log-select-pick))
    "Alist of auxiliary keys for `magit-log-select-mode-map'.")
  (al/bind-keys-from-vars 'magit-log-mode-map
    '(al/magit-history-keys al/magit-scroll-diff-keys)
    t)
  (al/bind-keys-from-vars 'magit-log-select-mode-map
    '(al/magit-moving-keys al/magit-log-select-keys)
    t)
  (al/bind-keys-from-vars 'magit-commit-section-map
    'al/magit-common-keys
    t))

(al/eval-after-load magit-diff
  (setq-default magit-diff-refine-hunk t)
  (defconst al/magit-diff-visit-keys
    '(("u" . magit-diff-visit-worktree-file)
      ("RET" . magit-diff-visit-worktree-file)
      ("<C-return>" . magit-diff-visit-file))
    "Alist of auxiliary keys for visiting files in `magit-diff-mode'.")
  (al/bind-keys-from-vars 'magit-diff-mode-map
    'al/magit-history-keys
    t)
  (al/bind-keys-from-vars 'magit-diff-section-map
    '(al/magit-common-keys al/magit-diff-visit-keys)
    t)
  (al/bind-keys-from-vars 'magit-staged-section-map 'al/magit-common-keys)
  (al/bind-key "u" magit-section-toggle magit-file-section-map))

(al/eval-after-load magit-sequence
  (transient-suffix-put 'magit-cherry-pick "A" :key "C") ; pick
  (transient-suffix-put 'magit-rebase "u" :key "r")      ; upstream
  )

(al/eval-after-load magit-remote
  (transient-suffix-put 'magit-remote "r" :key "R") ; rename
  )

(al/eval-after-load magit-push
  (transient-suffix-put 'magit-push "p" :key "P") ; push to remote
  )

(al/eval-after-load magit-pull
  (transient-suffix-put 'magit-pull "u" :key "F") ; pull from upstream
  )

(al/eval-after-load magit-fetch
  (transient-suffix-put 'magit-fetch "u" :key "f") ; fetch from upstream
  )

(al/eval-after-load magit-blame
  (setq magit-blame-time-format "%F")
  (defconst al/magit-blame-keys
    '(("."   . magit-blame-previous-chunk)
      ("e"   . magit-blame-next-chunk)
      ("M-." . magit-blame-previous-chunk-same-commit)
      ("M-e" . magit-blame-next-chunk-same-commit)
      ("M-k" . magit-blame-copy-hash))
    "Alist of auxiliary keys for `magit-blame-mode-map'.")
  (al/bind-keys-from-vars 'magit-blame-mode-map
    '(al/lazy-scrolling-keys al/magit-blame-keys)))

(al/eval-after-load magit-git
  (setq magit-git-executable "git"))

(al/eval-after-load git-commit
  (al/eval-at-hook git-commit-setup-hook
    ;; Not `git-commit-turn-on-flyspell' because it calls `flyspell-buffer'.
    (flyspell-mode)
    ;; `git-commit-setup-font-lock' spoils my `text-mode' syntax stuff.
    (modify-syntax-entry ?\" "\"   ")
    (al/no-syntactic-font-lock))

  (defconst al/git-commit-keys
    '(("M->" . git-commit-prev-message)
      ("M-E" . git-commit-next-message)
      ("C-c C-a" . al/git-commit-co-authored)
      ("C-c C-r" . git-commit-reported)
      ("C-c S" . git-commit-suggested))
    "Alist of auxiliary keys for `git-commit-mode-map'.")
  (al/bind-keys-from-vars 'git-commit-mode-map 'al/git-commit-keys))

(al/eval-after-load git-rebase
  (defconst al/git-rebase-keys
    '(("p"   . git-rebase-pick)
      ("w"   . git-rebase-reword)
      ("C-k" . git-rebase-kill-line)
      ("M-." . git-rebase-move-line-up)
      ("M-e" . git-rebase-move-line-down))
    "Alist of auxiliary keys for `git-rebase-mode-map'.")
  (al/bind-keys-from-vars 'git-rebase-mode-map 'al/git-rebase-keys)

  (add-hook 'git-rebase-mode-hook #'hl-line-mode))

(al/eval-after-load browse-at-remote
  (al/require al-browse-at-remote))

(al/eval-after-load al-browse-at-remote
  (advice-add 'browse-at-remote-get-url
    :around #'al/browse-at-remote-get-url))


;;; Misc settings and packages

(al/eval-after-load xref
  (setq xref-backend-functions '(elisp--xref-backend))
  (defconst al/xref-buffer-keys
    '(("." . xref-prev-line)
      ("e" . xref-next-line)
      ("u" . xref-goto-xref)
      ("d" . xref-show-location-at-point))
    "Alist of auxiliary keys for `xref--xref-buffer-mode-map'.")
  (al/bind-keys-from-vars 'xref--xref-buffer-mode-map
    'al/xref-buffer-keys))

(al/eval-after-load prog-mode
  (defconst al/prog-keys
    '(("<C-M-tab>" . prog-indent-sexp))
    "Alist of auxiliary keys for `prog-mode-map'.")
  (al/bind-keys-from-vars 'prog-mode-map 'al/prog-keys)

  (al/call-at-hook prog-mode-hook
    hl-line-mode
    hl-todo-mode
    abbrev-mode
    al/set-comment-column
    al/show-trailing-whitespace))

(al/eval-after-load cc-mode
  (setq
   c-default-style
   '((c-mode    . "stroustrup")
     (java-mode . "java")
     (awk-mode  . "awk")
     (other     . "gnu")))
  (defconst al/c-base-keys
    '(("<H-M-tab>" . c-indent-defun))
    "Alist of auxiliary keys for `c-mode-base-map'.")
  (al/bind-keys-from-vars 'c-mode-base-map
    '(al/prog-keys al/c-base-keys)))

(al/eval-after-load js
  (defconst al/js-keys
    '(("M-d" . js-find-symbol)
      ("C-c M-v" . js-eval)
      ("C-M-v" . js-eval-defun))
    "Alist of auxiliary keys for `js-mode-map'.")
  (al/bind-keys-from-vars 'js-mode-map 'al/js-keys)

  (al/eval-at-hook js-mode-hook
    (setq-local al/delimiter
                (concat (make-string 64 ?/) "\n///"))))

(al/autoload "python" python-shell-switch-to-shell)
(al/eval-after-load python
  (setq python-shell-interpreter "ipython")
  (defconst al/python-keys
    '(("C-v" . python-shell-send-region)
      ("C-M-v" . python-shell-send-defun)
      ("M-s-v" . python-shell-send-buffer))
    "Alist of auxiliary keys for `python-mode-map'.")
  (al/bind-keys-from-vars 'python-mode-map 'al/python-keys))

;;; prog.el ends here
