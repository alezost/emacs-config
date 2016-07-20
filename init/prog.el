;;; prog.el --- Programming modes and tools

;; Copyright © 2014-2016 Alex Kost

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

(require 'al-key)


;;; Working with elisp: eldoc, edebug, debugger, …

(setq eval-expression-print-length nil)

(put 'advice-add 'lisp-indent-function 1)

(al/bind-key* "M-v" al/pp-eval-expression)
(al/bind-keys
 ("C-v"   . al/eval-dwim)
 ("C-s-v" . al/pp-eval-dwim)
 ("C-S-v" . pp-macroexpand-last-sexp)
 ("C-M-v" . eval-defun)
 ("M-s-v" . eval-buffer)
 ("C-d"   . elisp-slime-nav-describe-elisp-thing-at-point)
 ("M-d"   . elisp-slime-nav-find-elisp-thing-at-point))
(al/bind-keys
 :prefix-map al/doc-map
 :prefix-docstring "Map for documentation/finding definitions."
 :prefix "C-M-d"
 ("f" . find-function)
 ("v" . find-variable)
 ("F" . find-face-definition)
 ("b" . describe-personal-keybindings))

(with-eval-after-load 'lisp-mode
  ;; (setq lisp-indent-function 'common-lisp-indent-function)
  (defconst al/lisp-shared-keys
    '(("<C-M-tab>" . al/indent-sexp))
    "Alist of auxiliary keys for `lisp-mode-shared-map'.")
  (al/bind-keys-from-vars 'lisp-mode-shared-map 'al/lisp-shared-keys)
  (al/bind-keys-from-vars
      '(emacs-lisp-mode-map
        lisp-interaction-mode-map
        lisp-mode-map))

  ;; XXX In 25.1 `emacs-lisp-mode-hook',
  ;; `emacs-lisp-mode-syntax-table', … are placed in elisp-mode.el
  (al/add-hook-maybe 'lisp-mode-hook 'al/imenu-add-sections)
  (al/add-hook-maybe
      '(emacs-lisp-mode-hook
        lisp-interaction-mode-hook)
    '(al/imenu-add-sections
      al/imenu-add-use-package
      al/imenu-add-eval-after-load))

  (if (version< emacs-version "25")
      (progn
        (al/modify-page-break-syntax emacs-lisp-mode-syntax-table)
        (al/modify-page-break-syntax lisp-mode-syntax-table))
    (al/modify-page-break-syntax lisp--mode-syntax-table)))

(with-eval-after-load 'ielm
  (setq ielm-prompt "EL> ")
  (al/bind-keys-from-vars 'ielm-map)
  (al/add-hook-maybe 'ielm-mode-hook 'al/no-truncate-lines))

;; XXX delete (In 25.1 there is `global-eldoc-mode' enabled by default).
(when (version< emacs-version "25")
  (al/add-hook-maybe
      '(emacs-lisp-mode-hook
        lisp-interaction-mode-hook
        ielm-mode-hook)
    'eldoc-mode))

(with-eval-after-load 'eldoc
  (when (version< emacs-version "25")
    (setq eldoc-argument-case 'al/eldoc-argument-list))
  (setq eldoc-idle-delay 0.3))

(with-eval-after-load 'edebug
  (al/bind-keys
   :map edebug-mode-map
   ("v"   . edebug-eval-expression)
   ("C-v" . edebug-eval-last-sexp)))

(al/bind-key "C-c d" toggle-debug-on-error)
(with-eval-after-load 'debug
  (al/bind-keys-from-vars 'debugger-mode-map 'al/button-keys t)
  (al/bind-keys
   :map debugger-mode-map
   ("v" . debugger-eval-expression)
   ("l" . debugger-toggle-locals)
   ("f" . debugger-list-functions)))


;;; SLIME

;; Use SLIME from quicklisp.
(let* ((quicklisp-dir  (expand-file-name "~/.quicklisp"))
       (swank.txt-file (expand-file-name
                        "dists/quicklisp/installed/systems/swank.txt"
                        quicklisp-dir)))
  (al/with-check
    :file swank.txt-file
    (let* ((swank.txt (with-temp-buffer
                        (insert-file-contents swank.txt-file)
                        (buffer-string)))
           (slime-dir (file-name-directory
                       (expand-file-name swank.txt quicklisp-dir))))
      (al/add-to-load-path-maybe slime-dir)
      (require 'slime-autoloads nil t))))
(setq slime-contribs '(slime-fancy))

;; `al/slime-keys' is required for `al/erc-channel-config'
(defconst al/slime-keys
  '(("C-v"     . al/slime-eval-dwim)
    ("C-M-v"   . slime-eval-defun)
    ("M-s-v"   . slime-eval-buffer)
    ("<M-tab>" . slime-complete-symbol)
    ("C-d"     . slime-describe-symbol)
    ("M-d"     . slime-edit-definition)
    ("C-M-d"   . slime-doc-map)
    "C-c C-d")
  "Alist of auxiliary keys for slime modes.")
(al/bind-keys
 :prefix-map al/slime-map
 :prefix-docstring "Map for slime commands."
 :prefix "M-L"
 ("l"   . slime-repl)
 ("M-L" . slime-repl)
 ("c"   . al/slime-stumpwm-connect)
 ("d"   . slime-disconnect)
 ("M-S" . slime)
 ("s"   . slime-selector))

(with-eval-after-load 'slime
  (setq
   inferior-lisp-program "sbcl"
   ;; slime-lisp-implementations
   ;; `((sbcl ("sbcl" "--core" ,(al/src-dir-file "sbcl-with-swank"))))
   ;; Do not ask about version difference.
   slime-protocol-version 'ignore)

  (al/bind-keys-from-vars 'slime-parent-map
    '(al/free-misc-keys al/slime-keys))
  (al/bind-keys-from-vars '(slime-mode-map slime-editing-map)))

(with-eval-after-load 'slime-repl
  (defconst al/slime-repl-keys
    '(("C-k" . al/slime-repl-kill-whole-line)
      ("M-." . slime-repl-previous-input)
      ("M-e" . slime-repl-next-input)
      ("M->" . slime-repl-previous-prompt)
      ("M-E" . slime-repl-next-prompt)
      ("M-r" . slime-repl-previous-matching-input))
    "Alist of auxiliary keys for `slime-repl-mode-map'.")
  (al/bind-keys-from-vars 'slime-repl-mode-map 'al/slime-repl-keys))

(with-eval-after-load 'slime-autodoc
  ;; `slime-autodoc-mode' binds some useless keys into "C-c C-d" prefix.
  (al/clean-map 'slime-autodoc-mode-map)
  (al/bind-keys
   :map slime-autodoc-mode-map
   ("SPC" . slime-autodoc-space)))


;;; Scheme, geiser

(with-eval-after-load 'scheme
  (put 'plist-new 'scheme-indent-function 1)
  (al/modify-page-break-syntax scheme-mode-syntax-table)
  (al/add-hook-maybe 'scheme-mode-hook
    '(al/imenu-add-sections guix-devel-mode))
  (when (require 'al-scheme nil t)
    (setq scheme-imenu-generic-expression
          al/scheme-imenu-generic-expression)
    (advice-add 'scheme-indent-function
      :override 'al/scheme-indent-function)))

(defconst al/geiser-keys
  '(("C-v"   . al/geiser-eval-dwim)
    ("C-S-v" . geiser-expand-last-sexp)
    ("C-M-v" . geiser-eval-definition)
    ("M-s-v" . geiser-eval-buffer)
    ("C-d"   . geiser-doc-symbol-at-point)
    ("M-d"   . geiser-edit-symbol-at-point)
    ("C-M-d" . al/geiser-doc-map)
    ("C-c a" . geiser-autodoc-mode)
    ("C-c j" . switch-to-geiser-module))
  "Alist of auxiliary keys for geiser modes.")

(with-eval-after-load 'geiser-mode
  (defvar al/geiser-doc-map)
  (put 'al/geiser-doc-map 'variable-documentation
       "Map for geiser documentation.")
  (define-prefix-command 'al/geiser-doc-map)
  (al/bind-keys
   :map al/geiser-doc-map
   ("d" . geiser-doc-symbol-at-point)
   ("i" . geiser-doc-look-up-manual)
   ("m" . geiser-doc-module)
   ("s" . geiser-autodoc-show)
   ("t" . geiser-autodoc-mode))
  (al/bind-keys-from-vars 'geiser-mode-map 'al/geiser-keys))

(with-eval-after-load 'geiser-repl
  (setq
   geiser-repl-skip-version-check-p t
   geiser-repl-use-other-window t
   geiser-repl-history-filename (al/emacs-data-dir-file "geiser-history"))

  (defconst al/geiser-repl-keys
    '(("<return>" . al/geiser-repl-enter-dwim)
      ("C-k"      . al/geiser-repl-kill-whole-line)
      ("C-a"      . geiser-repl--bol)
      ("C-c C-d"  . geiser-repl-exit)
      "C-c k")
    "Alist of auxiliary keys for `geiser-repl-mode'.")
  (al/bind-keys-from-vars 'geiser-repl-mode-map
    '(al/comint-keys al/geiser-keys al/geiser-repl-keys))

  (al/add-hook-maybe 'geiser-repl-mode-hook
    '(al/inhibit-field-motion
      al/no-syntactic-font-lock
      guix-build-log-minor-mode))

  (when (require 'al-geiser nil t)
    (setq geiser-repl-buffer-name-function
          #'al/geiser-repl-buffer-name)))

(with-eval-after-load 'geiser-impl
  (setq-default geiser-scheme-implementation 'guile)
  (setq geiser-active-implementations '(guile)))

(with-eval-after-load 'geiser-guile
  (setq geiser-guile-binary '("guile" "--no-auto-compile")))

(with-eval-after-load 'geiser-doc
  (defconst al/geiser-doc-keys
    '((","   . geiser-doc-previous)
      ("p"   . geiser-doc-next)
      ("C-d" . al/geiser-doc-doc-symbol-at-point)
      ("M-d" . geiser-doc-edit-symbol-at-point))
    "Alist of auxiliary keys for `geiser-doc-mode'.")
  (al/bind-keys-from-vars 'geiser-doc-mode-map
    '(al/button-keys al/geiser-keys al/geiser-doc-keys)))


;;; Compilation, Makefile

(with-eval-after-load 'make-mode
  (defconst al/make-keys
    '(("M->" . makefile-previous-dependency)
      ("M-E" . makefile-next-dependency))
    "Alist of auxiliary keys for `make-mode-map'.")
  (al/bind-keys-from-vars 'makefile-mode-map 'al/make-keys))

(with-eval-after-load 'compile
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
  (add-hook 'compilation-mode-hook 'hl-line-mode)

  (when (require 'al-compilation nil t)
    (al/add-hook-maybe 'compilation-finish-functions
      'al/compilation-notify)))

(with-eval-after-load 'al-compilation
  (setq
   al/compilation-sound-success (al/sound-dir-file "bell.oga")
   al/compilation-sound-error (al/sound-dir-file "splat.wav")))


;;; Version control

(setq vc-handled-backends nil)
(setq magit-auto-revert-mode nil)

(al/bind-keys
 :prefix-map al/magit-map
 :prefix-docstring "Map for magit and git stuff."
 :prefix "M-m"
 ("M-m" . al/magit-ido-switch-buffer)
 ("b" . magit-blame-popup)
 ("c" . magit-show-commit)
 ("s" . magit-status)
 ("l" . magit-log-current)
 ("u" . github-browse-file))

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
  '(("."   . magit-section-backward)
    ("e"   . magit-section-forward)
    ("M-." . magit-section-backward-sibling)
    ("M-e" . magit-section-forward-sibling))
  "Alist of auxiliary keys for moving by magit sections.")

(al/autoload "magit" ido-enter-magit-status)
(with-eval-after-load 'magit
  (setq
   magit-status-buffer-name-format   "*magit: %a*"
   magit-process-buffer-name-format  "*magit-process: %a*"
   magit-log-buffer-name-format      "*magit-log: %a*"
   magit-reflog-buffer-name-format   "*magit-reflog: %a*"
   magit-refs-buffer-name-format     "*magit-refs: %a*"
   magit-diff-buffer-name-format     "*magit-diff: %a*"
   magit-revision-buffer-name-format "*magit-revision: %a*"
   magit-cherry-buffer-name-format   "*magit-cherry: %a*"
   magit-stash-buffer-name-format    "*magit-stash: %a*"
   magit-stashes-buffer-name-format  "*magit-stashes: %a*")
  (setq
   magit-merge-arguments '("--ff-only")
   magit-push-always-verify t
   magit-branch-read-upstream-first nil)
  (magit-change-popup-key 'magit-branch-popup :action ?m ?R) ; rename
  )

(with-eval-after-load 'magit-mode
  (setq
   magit-save-repository-buffers nil
   magit-uniquify-buffer-names nil)

  (defconst al/magit-keys
    '(("<backtab>" . magit-section-cycle-global)
      ("H-SPC" . magit-diff-show-or-scroll-up)
      ("M-k" . magit-copy-section-value)
      ("u" . magit-show-commit)
      ("U" . magit-unstage-file)
      ("E" . magit-ediff-dwim)
      ("C" . magit-cherry-pick-popup)
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
      al/magit-keys)))

(with-eval-after-load 'magit-popup
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

  (al/add-hook-maybe 'magit-popup-mode-hook 'al/bar-cursor-type)

  ;; Move away from buttons.  Adding `al/beginning-of-buffer' to
  ;; `magit-popup-mode-hook' wouldn't work because
  ;; `magit-refresh-popup-buffer' is called after the mode is set.
  (advice-add 'magit-refresh-popup-buffer
    :after #'al/beginning-of-buffer))

(with-eval-after-load 'magit-log
  (setq
   magit-log-margin-spec '(25 1 magit-duration-spec)
   magit-reflog-arguments '("-n99")
   magit-log-arguments `(,@magit-reflog-arguments "--decorate")
   magit-log-select-arguments magit-log-arguments)
  (magit-change-popup-key 'magit-log-popup :option ?G ?p) ; patch

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

(with-eval-after-load 'magit-diff
  (defconst al/magit-diff-visit-keys
    '(("RET" . magit-diff-visit-file-worktree)
      ("<C-return>" . magit-diff-visit-file))
    "Alist of auxiliary keys for visiting files in `magit-diff-mode'.")
  (al/bind-keys-from-vars 'magit-diff-mode-map
    'al/magit-history-keys
    t)
  (al/bind-keys-from-vars 'magit-file-section-map
    '(al/magit-common-keys al/magit-diff-visit-keys)
    t)
  (al/bind-keys-from-vars 'magit-hunk-section-map
    '(al/magit-common-keys al/magit-diff-visit-keys)
    t)
  (al/bind-keys-from-vars 'magit-staged-section-map 'al/magit-common-keys))

(with-eval-after-load 'magit-sequence
  (magit-change-popup-key 'magit-cherry-pick-popup :action ?A ?C) ; pick
  (magit-change-popup-key 'magit-rebase-popup :action ?u ?r) ; upstream
  )

(with-eval-after-load 'magit-bisect
  (magit-change-popup-key 'magit-bisect-popup :action ?B ?s) ; start
  (magit-change-popup-key 'magit-bisect-popup :action ?s ?!) ; run script
  (magit-change-popup-key 'magit-bisect-popup
                          :sequence-action ?s ?!) ; run script
  )

(with-eval-after-load 'magit-remote
  (magit-change-popup-key 'magit-remote-popup :action ?r ?R) ; rename
  (magit-change-popup-key 'magit-push-popup :action ?p ?P) ; pushRemote
  (magit-change-popup-key 'magit-pull-popup :action ?u ?F) ; upstream
  (magit-change-popup-key 'magit-fetch-popup :action ?u ?f) ; upstream
  )

(with-eval-after-load 'magit-blame
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

(with-eval-after-load 'magit-git
  (setq magit-git-executable "git"))

(with-eval-after-load 'git-commit
  (defun al/git-commit-fix-syntax ()
    (modify-syntax-entry ?\" "\"   ")
    (al/no-syntactic-font-lock))
  (al/add-hook-maybe 'git-commit-setup-hook
    '(;; Not `git-commit-turn-on-flyspell' because it calls `flyspell-buffer'.
      flyspell-mode
      ;; `git-commit-setup-font-lock' spoils my `text-mode' syntax stuff.
      al/git-commit-fix-syntax))

  (defconst al/git-commit-keys
    '(("M->" . git-commit-prev-message)
      ("M-E" . git-commit-next-message)
      ("C-c C-a" . al/git-commit-co-authored)
      ("C-c C-r" . git-commit-reported)
      ("C-c S" . git-commit-suggested))
    "Alist of auxiliary keys for `git-commit-mode-map'.")
  (al/bind-keys-from-vars 'git-commit-mode-map 'al/git-commit-keys))

(with-eval-after-load 'git-rebase
  (al/add-hook-maybe 'git-rebase-mode-hook 'hl-line-mode)
  (defconst al/git-rebase-keys
    '(("p"   . git-rebase-pick)
      ("w"   . git-rebase-reword)
      ("C-k" . git-rebase-kill-line)
      ("M-." . git-rebase-move-line-up)
      ("M-e" . git-rebase-move-line-down))
    "Alist of auxiliary keys for `git-rebase-mode-map'.")
  (al/bind-keys-from-vars 'git-rebase-mode-map 'al/git-rebase-keys))


;;; Misc settings and packages

(with-eval-after-load 'prog-mode
  (al/add-hook-maybe 'prog-mode-hook
    '(hl-line-mode
      hl-todo-mode
      ;; indent-guide-mode
      abbrev-mode
      al/set-comment-column
      al/show-trailing-whitespace))
  (defconst al/prog-keys
    '(("<C-M-tab>" . prog-indent-sexp))
    "Alist of auxiliary keys for `prog-mode-map'.")
  (al/bind-keys-from-vars 'prog-mode-map 'al/prog-keys))

(with-eval-after-load 'cc-mode
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

(with-eval-after-load 'js
  (defconst al/js-keys
    '(("M-d" . js-find-symbol)
      ("C-c M-v" . js-eval)
      ("C-M-v" . js-eval-defun))
    "Alist of auxiliary keys for `js-mode-map'.")
  (al/bind-keys-from-vars 'js-mode-map 'al/js-keys)
  (defun al/js-delimiter ()
    (setq-local al/delimiter
                (concat (make-string 64 ?/) "\n///")))
  (al/add-hook-maybe 'js-mode-hook
    '(al/imenu-add-js-sections al/js-delimiter)))

(al/autoload "python" python-shell-switch-to-shell)
(with-eval-after-load 'python
  (setq python-shell-interpreter "ipython")
  (defconst al/python-keys
    '(("C-v" . python-shell-send-region)
      ("C-M-v" . python-shell-send-defun)
      ("M-s-v" . python-shell-send-buffer))
    "Alist of auxiliary keys for `python-mode-map'.")
  (al/bind-keys-from-vars 'python-mode-map 'al/python-keys))

;;; prog.el ends here
