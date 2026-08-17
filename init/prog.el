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

(declare-function al/magit-switch-buffer "al-magit")


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
 magit-define-global-key-bindings nil

 ;; By default, when `with-editor' library is loaded, it runs
 ;; "<emacsclient> --version" shell command HUNDREDS of times (for any
 ;; possible name of <emacsclient> executable in all dirs from PATH).
 ;; This happens during initializing `with-editor-emacsclient-executable'
 ;; variable (when `with-editor-locate-emacsclient' is called).
 with-editor-emacsclient-executable
 (expand-file-name "emacsclient" invocation-directory)

 ;; `magit-log-margin' should be set before magit is loaded, as
 ;; the other margins are defined from this one.
 magit-log-margin '(t age-abbreviated magit-log-margin-width t 20))

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

(al/eval-after-load magit-mode
  (al/load-settings "magit"))

(al/eval-after-load magit-popup
  (al/load-settings "magit-popup"))


;;; Misc settings and packages

(al/eval-after-load xref
  (al/load-settings "xref"))

(al/eval-after-load prog-mode
  (al/bind-keys
    :map prog-mode-map
    ("<C-M-tab>" . prog-indent-sexp))

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
