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
  (al/load-settings "ielm"))

(al/eval-after-load eldoc
  (setq eldoc-idle-delay 0.3))

(al/eval-after-load edebug
  (al/load-settings "edebug"))

(al/bind-key "C-c d" toggle-debug-on-error)
(al/eval-after-load debug
  (al/load-settings "debug"))

(al/eval-after-load ert
  (al/load-settings "ert"))

(al/eval-after-load pp
  (al/load-settings "pp"))


;;; SLY

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
  (al/load-settings "sly"))


;;; Scheme, geiser

(al/eval-after-load scheme
  (al/load-settings "scheme"))

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
  (al/load-settings "geiser"))


;;; Haskell

(al/eval-after-load haskell-mode
  (al/load-settings "haskell-mode"))


;;; GDB, GUD

(al/setq-no-warnings gud-key-prefix (key-parse "M-G"))

(al/eval-after-load gud
  (al/load-settings "gud"))


;;; Compilation, Makefile

(al/eval-after-load make-mode
  (al/load-settings "make-mode"))

(al/eval-after-load compile
  (al/load-settings "compile"))


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
  (al/load-settings "xref"))

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
  (al/load-settings "cc-mode"))

(al/eval-after-load js
  (al/load-settings "js"))

(al/autoload "python" python-shell-switch-to-shell)
(al/eval-after-load python
  (al/load-settings "python"))

;;; prog.el ends here
