;;; geiser.el --- Settings for `geiser' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'geiser-mode)
(require 'al-geiser)
(require 'al-key)

(al/bind-keys
  :map al/geiser-doc-map
  :create t
  ("d" 'geiser-doc-symbol-at-point)
  ("i" 'geiser-doc-look-up-manual)
  ("m" 'geiser-doc-module)
  ("s" 'geiser-autodoc-show)
  ("t" 'geiser-autodoc-mode))

(al/bind-keys
  ;; Do not remove this map (used by ERC settings).
  :map al/geiser-map
  :create t
  ("C-v"   'al/geiser-eval-dwim)
  ("C-S-v" 'geiser-expand-last-sexp)
  ("C-M-v" 'geiser-eval-definition)
  ("M-s-v" 'geiser-eval-buffer)
  ("C-d"   'geiser-doc-symbol-at-point)
  ("M-d"   'geiser-edit-symbol-at-point)
  ("C-M-d" al/geiser-doc-map)
  ("C-c l" 'al/geiser-add-to-load-path)
  ("C-c a" 'geiser-autodoc-mode)
  ("C-c j" 'switch-to-geiser-module)
  ;; Although this "C-c C-z" exists in `geiser-mode-map',
  ;; it is bound here for ERC channel buffers.
  ("C-c C-z" 'geiser-mode-switch-to-repl)
  ("C-c C-j" 'geiser-mode-switch-to-repl-and-enter))

(al/bind-keys
  :map geiser-mode-map
  :parent al/geiser-map)

(al/bind-keys
  :map geiser-repl-mode-map
  :parent (al/geiser-map
           comint-mode-map)
  "C-c k"
  ("RET" 'al/geiser-repl-enter-dwim)
  ("C-k" 'al/geiser-repl-kill-whole-line)
  ("C-⇤" 'geiser-repl--bol)
  ("C-c C-d" 'geiser-repl-exit))

(al/bind-keys
  :map geiser-doc-mode-map
  :parent (al/geiser-map
           button-buffer-map)
  ("↷" 'geiser-doc-previous)
  ("↶" 'geiser-doc-next)
  ("C-d" 'al/geiser-doc-doc-symbol-at-point)
  ("M-d" 'geiser-doc-edit-symbol-at-point))

(setq
 geiser-repl-skip-version-check-p t
 geiser-repl-use-other-window t
 geiser-repl-history-filename (al/emacs-data-dir-file "geiser-history")
 geiser-active-implementations '(guile racket))

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
         (expand-file-name "shepherd/repl" xdg-dir))))

(setq-default geiser-scheme-implementation 'guile)

(geiser-implementation-extension 'racket "rkt[dl]?")

(al/call-at-hook geiser-repl-mode-hook
  al/inhibit-field-motion
  al/no-syntactic-font-lock)

(advice-add 'geiser-debug-mode :override 'scheme-mode)

;;; geiser.el ends here
