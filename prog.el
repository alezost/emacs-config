;;; prog.el --- Programming modes and tools

;; Copyright © 2014-2015 Alex Kost

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


;;; Working with elisp: eldoc, edebug, debugger, …

(setq eval-expression-print-length nil)

(put 'advice-add 'lisp-indent-function 1)

(bind-key* "M-v" 'utl-pp-eval-expression)
(bind-keys
 ("C-v"   . utl-eval-dwim)
 ("C-s-v" . utl-pp-eval-dwim)
 ("C-S-v" . pp-macroexpand-last-sexp)
 ("C-M-v" . eval-defun)
 ("M-s-v" . eval-buffer)
 ("C-d"   . elisp-slime-nav-describe-elisp-thing-at-point)
 ("M-d"   . elisp-slime-nav-find-elisp-thing-at-point))
(bind-keys
 :prefix-map al/doc-map
 :prefix-docstring "Map for documentation/finding definitions."
 :prefix "C-M-d"
 ("f" . find-function)
 ("v" . find-variable)
 ("F" . find-face-definition)
 ("b" . describe-personal-keybindings))

(use-package lisp-mode
  :defer t
  :config
  ;; (setq lisp-indent-function 'common-lisp-indent-function)
  (al/bind-keys-from-vars
      '(lisp-mode-shared-map
        emacs-lisp-mode-map
        lisp-interaction-mode-map
        lisp-mode-map))
  (bind-key "<C-M-tab>" 'utl-indent-sexp lisp-mode-shared-map)

  ;; XXX In 25.1 `emacs-lisp-mode-hook',
  ;; `emacs-lisp-mode-syntax-table', … are placed in elisp-mode.el
  (al/add-hook-maybe 'lisp-mode-hook
    '(utl-imenu-add-sections paredit-mode))
  (al/add-hook-maybe
      '(emacs-lisp-mode-hook
        lisp-interaction-mode-hook)
    '(utl-imenu-add-use-package utl-imenu-add-sections paredit-mode))

  (if (version< emacs-version "25")
      (progn
        (al/modify-page-break-syntax 'emacs-lisp-mode-syntax-table)
        (al/modify-page-break-syntax 'lisp-mode-syntax-table))
    (al/modify-page-break-syntax 'lisp--mode-syntax-table)))

(use-package ielm
  :defer t
  :config
  (setq ielm-prompt "EL> ")
  (al/bind-keys-from-vars 'ielm-map)
  (al/add-hook-maybe 'ielm-mode-hook
    '(al/no-truncate-lines paredit-mode)))

(use-package eldoc
  :defer t
  :diminish ""
  :init
  ;; XXX delete (In 25.1 there is `global-eldoc-mode' enabled by default).
  (when (version< emacs-version "25")
    (al/add-hook-maybe
        '(emacs-lisp-mode-hook
          lisp-interaction-mode-hook
          ielm-mode-hook)
      'eldoc-mode))
  :config
  (when (version< emacs-version "25")
    (setq eldoc-argument-case 'utl-eldoc-argument-list))
  (setq eldoc-idle-delay 0.3))

(use-package edebug
  :defer t
  :diminish " 🔧"
  :config
  (bind-keys
   :map edebug-mode-map
   ("v"   . edebug-eval-expression)
   ("C-v" . edebug-eval-last-sexp)))

(use-package debug
  :defer t
  :init
  (bind-key "C-c d" 'toggle-debug-on-error)
  :config
  (al/bind-keys-from-vars 'debugger-mode-map 'al/button-keys t)
  (bind-keys
   :map debugger-mode-map
   ("v" . debugger-eval-expression)
   ("l" . debugger-toggle-locals)
   ("f" . debugger-list-functions)))


;;; SLIME

(use-package slime
  :defer t
  :init
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
    '(("C-v"     . utl-slime-eval-dwim)
      ("C-M-v"   . slime-eval-defun)
      ("M-s-v"   . slime-eval-buffer)
      ("<M-tab>" . slime-complete-symbol)
      ("C-d"     . slime-describe-symbol)
      ("M-d"     . slime-edit-definition)
      ("C-M-d"   . slime-doc-map)
      "C-c C-d")
    "Alist of auxiliary keys for slime modes.")
  (bind-keys
   :prefix-map al/slime-map
   :prefix-docstring "Map for slime commands."
   :prefix "M-L"
   ("l"   . slime-repl)
   ("M-L" . slime-repl)
   ("c"   . (lambda () (interactive)
              (slime-connect slime-lisp-host slime-port)))
   ("d"   . slime-disconnect)
   ("M-S" . slime)
   ("s"   . slime-selector))

  :config
  (setq
   inferior-lisp-program "sbcl"
   ;; slime-lisp-implementations
   ;; `((sbcl ("sbcl" "--core" ,(al/src-dir-file "sbcl-with-swank"))))
   ;; Do not ask about version difference.
   slime-protocol-version 'ignore)

  (al/bind-keys-from-vars 'slime-parent-map 'al/slime-keys)
  (al/bind-keys-from-vars 'slime-editing-map))

(use-package slime-repl
  :defer t
  :config
  (bind-keys
   :map slime-repl-mode-map
   ("C-k" . utl-slime-repl-kill-whole-line)
   ("M-." . slime-repl-previous-input)
   ("M-e" . slime-repl-next-input)
   ("M->" . slime-repl-previous-prompt)
   ("M-E" . slime-repl-next-prompt)
   ("M-r" . slime-repl-previous-matching-input))
  (al/add-hook-maybe 'slime-repl-mode-hook 'paredit-mode))

(use-package slime-autodoc
  :defer t
  :config
  ;; `slime-autodoc-mode' binds some useless keys into "C-c C-d" prefix.
  (al/clean-map 'slime-autodoc-mode-map)
  (bind-keys
   :map slime-autodoc-mode-map
   ("SPC" . slime-autodoc-space)))


;;; Scheme, geiser

(use-package scheme
  :defer t
  :config
  (put 'plist-new 'scheme-indent-function 1)
  (al/modify-page-break-syntax 'scheme-mode-syntax-table)
  (al/add-hook-maybe 'scheme-mode-hook
    '(utl-imenu-add-sections paredit-mode))
  (when (require 'utl-scheme nil t)
    (setq scheme-imenu-generic-expression
          utl-scheme-imenu-generic-expression)
    (advice-add 'scheme-indent-function
      :override 'utl-scheme-indent-function)))

(use-package geiser-mode
  :defer t
  :init
  ;; `al/geiser-keys' is required for `al/erc-channel-config'
  (defconst al/geiser-keys
    '(("C-v"   . utl-geiser-eval-dwim)
      ("C-M-v" . geiser-eval-definition)
      ("M-s-v" . geiser-eval-buffer)
      ("C-d"   . geiser-doc-symbol-at-point)
      ("M-d"   . geiser-edit-symbol-at-point)
      ("C-M-d" . al/geiser-doc-map))
    "Alist of auxiliary keys for geiser modes.")

  :config
  (setq-default geiser-scheme-implementation 'guile)
  (setq
   geiser-autodoc-delay 0.5
   geiser-guile-binary '("guile" "--no-auto-compile"))
  (defvar al/geiser-doc-map)
  (put 'al/geiser-doc-map 'variable-documentation
       "Map for geiser documentation.")
  (define-prefix-command 'al/geiser-doc-map)
  (bind-keys
   :map al/geiser-doc-map
   ("d" . geiser-doc-symbol-at-point)
   ("i" . geiser-doc-look-up-manual)
   ("m" . geiser-doc-module)
   ("s" . geiser-autodoc-show)
   ("t" . geiser-autodoc-mode))
  (al/bind-keys-from-vars 'geiser-mode-map 'al/geiser-keys)

  ;; Do not put the code below into “(use-package geiser-repl …)”
  ;; because Geiser applies its bindings after providing `geiser-repl'
  ;; feature somehow, and an error telling about non-prefix key happens
  ;; (as it tries to bind "C-c C-d <smth>" over my "C-c C-d").
  (defconst al/geiser-repl-keys
    '(("<return>" . utl-geiser-repl-enter-dwim)
      ("C-k"      . utl-geiser-repl-kill-whole-line)
      ("C-a"      . geiser-repl--bol)
      ("C-c C-d"  . geiser-repl-exit)
      "C-c k")
    "Alist of auxiliary keys for `geiser-repl-mode'.")
  (al/bind-keys-from-vars 'geiser-repl-mode-map
    '(al/comint-keys al/geiser-keys al/geiser-repl-keys)))

(use-package geiser-repl
  :defer t
  :config
  (setq
   geiser-repl-use-other-window t
   geiser-repl-history-filename (al/emacs-data-dir-file ".geiser_history"))
  (al/add-hook-maybe 'geiser-repl-mode-hook
    '(paredit-mode al/inhibit-field-motion)))

(use-package geiser-doc
  :defer t
  :config
  (defconst al/geiser-doc-keys
    '((","   . geiser-doc-previous)
      ("p"   . geiser-doc-next)
      ("C-d" . utl-geiser-doc-doc-symbol-at-point)
      ("M-d" . geiser-doc-edit-symbol-at-point))
    "Alist of auxiliary keys for `geiser-doc-mode'.")
  (al/bind-keys-from-vars 'geiser-doc-mode-map
    '(al/button-keys al/geiser-keys al/geiser-doc-keys)))


;;; Compilation, Makefile

(use-package make-mode
  :defer t
  :config
  (defconst al/make-keys
    '(("M->" . makefile-previous-dependency)
      ("M-E" . makefile-next-dependency))
    "Alist of auxiliary keys for `make-mode-map'.")
  (al/bind-keys-from-vars 'makefile-mode-map 'al/make-keys))

(use-package compile
  :defer t
  :config
  (setq
   ;; Don't ask, don't save.
   compilation-ask-about-save nil
   compilation-save-buffers-predicate 'ignore)
  (defconst al/compilation-keys
    '(("."   . compilation-previous-error)
      ("e"   . compilation-next-error)
      ("u"   . compile-goto-error)
      ("M-." . previous-error-no-select)
      ("M-e" . next-error-no-select))
    "Alist of auxiliary keys for compilation modes.")
  (al/bind-keys-from-vars
      '(compilation-mode-map compilation-minor-mode-map)
    'al/compilation-keys)
  (add-hook 'compilation-mode-hook 'hl-line-mode)

  (when (require 'utl-compilation nil t)
    (al/add-hook-maybe 'compilation-finish-functions
      'utl-compilation-notify)))

(use-package utl-compilation
  :defer t
  :config
  (setq
   utl-compilation-sound-success (al/sound-dir-file "bell.oga")
   utl-compilation-sound-error (al/sound-dir-file "splat.wav")))


;;; Version control

(setq vc-handled-backends nil)

(bind-keys
 :prefix-map al/magit-map
 :prefix-docstring "Map for magit and git stuff."
 :prefix "M-m"
 ("M-m" . utl-magit-ido-switch-buffer)
 ("b" . magit-blame-popup)
 ("c" . magit-show-commit)
 ("s" . magit-status)
 ("l" . magit-log-current)
 ("k" . (lambda () (interactive)
          (let (github-browse-file-visit-url)
            (github-browse-file))))
 ("g" . github-browse-file))

(use-package magit
  :defer t
  :commands ido-enter-magit-status
  :config
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
  (setq magit-revert-buffers nil)
  (when (require 'utl-magit nil t)
    (utl-magit-popup-substitute-key
     'magit-branch-popup :actions ?u ?U) ; set upstream
    ))

(use-package magit-mode
  :defer t
  :config
  (setq magit-save-repository-buffers nil)
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
  (defconst al/magit-keys
    '(("<backtab>" . magit-section-cycle-global)
      ("H-SPC" . magit-diff-show-or-scroll-up)
      ("M-k" . magit-copy-as-kill)
      ("u" . magit-show-commit)
      ("U" . magit-unstage-file)
      ("E" . magit-ediff-dwim)
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

(use-package magit-popup
  :defer t
  :config
  (setq
   magit-popup-show-common-commands nil
   magit-popup-use-prefix-argument 'default)

  (defconst al/magit-popup-keys
    '(("M-." . backward-button)
      ("M-e" . forward-button)
      ("M-h" . magit-popup-toggle-show-common-commands))
    "Alist of auxiliary keys for `magit-popup-mode-map'.")
  (al/bind-keys-from-vars 'magit-popup-mode-map
    'al/magit-popup-keys)

  (al/add-hook-maybe 'magit-popup-mode-hook 'al/bar-cursor-type)

  ;; Move away from buttons.  Adding `al/beginning-of-buffer' to
  ;; `magit-popup-mode-hook' wouldn't work because
  ;; `magit-refresh-popup-buffer' is called after the mode is set.
  (advice-add 'magit-refresh-popup-buffer
    :after #'al/beginning-of-buffer))

(use-package magit-log
  :defer t
  :config
  (setq
   magit-log-margin-spec '(25 1 magit-duration-spec)
   magit-log-arguments '("--decorate"))
  (when (require 'utl-magit nil t)
    (utl-magit-popup-substitute-key
     'magit-log-popup :options ?m ?g)) ; grep

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

(use-package magit-diff
  :defer t
  :config
  (al/bind-keys-from-vars 'magit-diff-mode-map 'al/magit-history-keys)
  (al/bind-keys-from-vars 'magit-file-section-map 'al/magit-common-keys)
  (al/bind-keys-from-vars 'magit-hunk-section-map 'al/magit-common-keys)
  (al/bind-keys-from-vars 'magit-staged-section-map 'al/magit-common-keys))

(use-package magit-remote
  :defer t
  :config
  (when (require 'utl-magit nil t)
    (utl-magit-popup-substitute-key
     'magit-remote-popup :actions ?u ?s) ; set url
    (utl-magit-popup-substitute-key
     'magit-push-popup :actions ?e ?E) ; elsewhere
    ))

(use-package magit-sequence
  :defer t
  :config
  (when (require 'utl-magit nil t)
    (utl-magit-popup-substitute-key
     'magit-rebase-popup :actions ?e ?i) ; interactive
    ))

(use-package magit-bisect
  :defer t
  :config
  (when (require 'utl-magit nil t)
    (utl-magit-popup-substitute-key
     'magit-bisect-popup :actions ?a ?!) ; run
    (utl-magit-popup-substitute-key
     'magit-bisect-popup :actions ?B ?s) ; start
    ))

(use-package git-commit
  :defer t
  :config
  (al/add-hook-maybe 'git-commit-setup-hook
    ;; Not `git-commit-turn-on-flyspell' because it calls
    ;; `flyspell-buffer'.
    'flyspell-mode)
  (defconst al/git-commit-keys
    '(("M->" . git-commit-prev-message)
      ("M-E" . git-commit-next-message))
    "Alist of auxiliary keys for `git-commit-mode-map'.")
  (al/bind-keys-from-vars 'git-commit-mode-map 'al/git-commit-keys))

(use-package git-rebase
  :defer t
  :config
  (defconst al/git-rebase-keys
    '(("p"   . git-rebase-pick)
      ("C-k" . git-rebase-kill-line)
      ("M-." . git-rebase-move-line-up)
      ("M-e" . git-rebase-move-line-down))
    "Alist of auxiliary keys for `git-rebase-mode-map'.")
  (al/bind-keys-from-vars 'git-rebase-mode-map 'al/git-rebase-keys))


;;; Misc settings and packages

(setq c-default-style
      '((c-mode    . "stroustrup")
        (java-mode . "java")
        (awk-mode  . "awk")
        (other     . "gnu")))

(use-package prog-mode
  :defer t
  :config
  (al/add-hook-maybe 'prog-mode-hook
    '(hl-line-mode
      hl-todo-mode
      ;; indent-guide-mode
      abbrev-mode
      al/show-trailing-whitespace))
  (defconst al/prog-keys
    '(("<C-M-tab>" . prog-indent-sexp))
    "Alist of auxiliary keys for `prog-mode-map'.")
  (al/bind-keys-from-vars 'prog-mode-map 'al/prog-keys))

(use-package js
  :defer t
  :config
  (defun al/js-delimiter ()
    (setq-local utl-delimiter
                (concat (make-string 64 ?/) "\n///")))
  (al/add-hook-maybe 'js-mode-hook
    '(utl-imenu-add-js-sections al/js-delimiter)))

(use-package python
  :defer t
  :commands python-shell-switch-to-shell
  :config
  (setq python-shell-interpreter "ipython"))

;;; prog.el ends here
