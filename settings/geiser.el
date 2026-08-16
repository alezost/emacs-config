;;; geiser.el --- Settings for `geiser' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'geiser-mode)
(require 'al-geiser)
(require 'al-key)

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
  '(al/comint-keys al/geiser-keys al/geiser-repl-keys))

(defconst al/geiser-doc-keys
  '((","   . geiser-doc-previous)
    ("p"   . geiser-doc-next)
    ("C-d" . al/geiser-doc-doc-symbol-at-point)
    ("M-d" . geiser-doc-edit-symbol-at-point))
  "Alist of auxiliary keys for `geiser-doc-mode'.")
(al/bind-keys-from-vars 'geiser-doc-mode-map
  '(al/button-keys al/geiser-keys al/geiser-doc-keys))

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
