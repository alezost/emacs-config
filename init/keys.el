;;; keys.el --- Miscellaneous global key bindings and relative settings

;; Copyright © 2013-2015 Alex Kost

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


;;; Frame specific settings

(defun al/frame-keys-actions (&optional frame)
  "Configure key bindings specific to a FRAME type."
  (keyboard-translate ?\C-x ?\C-t)
  (keyboard-translate ?\C-t ?\C-x)
  (when (display-graphic-p)
    (keyboard-translate ?\C-m ?\C-ь)
    (keyboard-translate ?\C-i ?\C-п)))

(al/add-hook-maybe
    '(after-make-frame-functions window-setup-hook)
  'al/frame-keys-actions)


;;; Keys for multiple maps

(defconst al/free-moving-keys
  '("C-o" "M-o" "C-M-o" "M-O"
    "C-u" "M-u" "C-M-u" "M-U"
    "C-." "M-." "C-M-." "M->"
    "C-e" "M-e" "C-M-e" "M-E"
    "C-a" "M-a" "C-M-a" "M-A"
    "C-п" "M-i" "C-M-i" "M-I")
  "Alist of moving keys that should be unbound.")

(defconst al/free-editing-keys
  '("C-," "M-," "C-M-," "M-<"
    "C-p" "M-p" "C-M-p" "M-P"
                "C-M-q" "M-Q"
    "C-k" "M-k" "C-M-k" "M-K"
    "C-'" "M-'" "C-M-'" "M-\""
    "C-;" "M-;" "C-M-;"
    "C-t" "M-x" "C-M-x")
  "Alist of editing keys that should be unbound.")

(defconst al/free-important-keys
  '("M-g")
  "Alist of important keys that should be unbound.")

(defconst al/free-misc-keys
  '("M-_" "M-+"
    "M-/" "C-M-/" "M-?"
    "C-w"
    "C-M-t" "<C-tab>" "<M-tab>" "<C-M-tab>")
  "Alist of miscellaneous keys that should be unbound.")

(defconst al/lazy-moving-keys
  '(("o" . backward-char)
    ("u" . forward-char)
    ("." . previous-line)
    ("e" . next-line))
  "Alist of auxiliary keys for lazy moving.")

(defconst al/lazy-scrolling-keys
  '(("SPC" . scroll-up-command)
    ("DEL" . scroll-down-command))
  "Alist of auxiliary keys for lazy scrolling.")

(defconst al/text-editing-keys
  (when al/mwim-exists?
    '(("C-a" . mwim-beginning-of-line-or-code)
      ("C-п" . mwim-end-of-line-or-code)))
  "Alist of auxiliary keys for editing in text modes.")

(defconst al/button-keys
  '(("." . backward-button)
    ("e" . forward-button))
  "Alist of auxiliary keys for modes with buttons.")

(defconst al/minibuffer-keys
  '(("M-." . previous-history-element)
    ("M-e" . next-history-element))
  "Alist of auxiliary keys for minibuffer modes.")


;;; Binding keys

;; `al/bind-keys' macro and related code originates from `bind-key'
;; package: <https://github.com/jwiegley/use-package>.

(defconst al/default-keys-variables
  '(al/free-moving-keys al/free-editing-keys al/free-important-keys)
  "Default list of variables used by `al/bind-keys-from-vars'.")

(defvar al/override-global-map (make-keymap)
  "Keymap with key bindings to take precedence over other keymaps.")

(define-minor-mode al/override-global-mode
  "Minor mode with key bindings to override other modes."
  t "")

(add-to-list 'emulation-mode-map-alists
             `((al/override-global-mode . ,al/override-global-map)))

(defun al/key-command (cmd-spec)
  "Return command value for `al/bind-key' macro."
  (cond ((null cmd-spec) nil)
        ((listp cmd-spec)
         (if (eq (car cmd-spec) 'lambda)
             cmd-spec
           `(lambda () (interactive) ,@cmd-spec)))
        (t `',cmd-spec)))

(defmacro al/bind-key (key-name command &optional keymap)
  "Bind KEY-NAME to COMMAND in KEYMAP.

KEY-NAME should be a string taken by `read-kbd-macro'.

COMMAND may be either:

  - nil (to unbind the key if it is already bound in KEYMAP),
  - a command name (an unquoted symbol),
  - or a list (it will be wrapped into interactive `lambda' form).

If KEYMAP is not specified, use `global-map'.

Examples:

  (al/bind-key \"C-f\" nil)
  (al/bind-key \"C-j\" newline lisp-mode-map)
  (al/bind-key \"C-s-b\" ((backward-word) (backward-char)))"
  (let ((command (al/key-command command))
        (key-var (make-symbol "key"))
        (map-var (make-symbol "map")))
    `(let* ((,key-var (read-kbd-macro ,key-name))
            (,map-var (or ,keymap global-map)))
       ,(if command
            `(define-key ,map-var ,key-var ,command)
          `(when (lookup-key ,map-var ,key-var)
             (define-key ,map-var ,key-var nil))))))

(defmacro al/bind-key* (key-name command)
  `(al/bind-key ,key-name ,command al/override-global-map))

(defmacro al/bind-keys (&rest args)
  "Bind multiple keys.

ARGS are keyword arguments and key specifications.  The following
optional keywords are available:

  - `:map' - a keymap into which the key bindings should be added.

  - `:prefix-map' - name of a prefix map that should be created
    for these bindings.

  - `:prefix' - prefix key for these bindings.

  - `:prefix-docstring' - docstring of the prefix map variable.

The rest ARGS are conses of key binding strings and functions.
See `al/bind-key' for details."
  (let* ((map        (plist-get args :map))
         (doc        (plist-get args :prefix-docstring))
         (prefix-map (plist-get args :prefix-map))
         (prefix     (plist-get args :prefix))
         (bindings   (progn
                       (while (keywordp (car args))
                         (pop args)
                         (pop args))
                       args)))
    (or (and prefix prefix-map)
        (and (not prefix) (not prefix-map))
        (error "Both :prefix-map and :prefix must be supplied"))
    `(progn
       ,(when prefix-map
          `(progn
             (defvar ,prefix-map)
             ,(when doc
                `(put ',prefix-map 'variable-documentation ,doc))
             (define-prefix-command ',prefix-map)
             (al/bind-key ,prefix ,prefix-map ,map)))
       ,@(mapcar (lambda (form)
                   `(al/bind-key ,(car form) ,(cdr form)
                                 ,(or prefix-map map)))
                 bindings))))

(defmacro al/bind-keys* (&rest args)
  `(al/bind-keys :map al/override-global-map ,@args))

(defun al/bind-keys-to-map (key-specs map-var)
  "Bind all keys from KEY-SPECS in MAP-VAR.
KEY-SPECS is an alist of keybinding strings and functions (the
same as the rest of arguments taken by `al/bind-keys').
MAP-VAR is a variable with keymap."
  (al/with-check
    :var map-var
    (dolist (spec key-specs)
      (let ((key (car spec))
            (cmd (cdr spec)))
        (eval `(al/bind-key ,key ,cmd ,map-var))))))

(defun al/keys-from-vars (vars)
  "Return list of key binding specifications from variables VARS.
For the meaning of values of VARS, see `al/bind-keys-from-vars'.
Returning value is an alist of keys and functions with removed
key duplicates (rightmost values retain)."
  (let* ((vars (cl-remove-if-not #'al/bound? vars))
         (keys-raw (apply #'append
                          (mapcar #'symbol-value vars)))
         (keys (mapcar #'al/list-maybe keys-raw)))
    (cl-remove-duplicates
     keys
     :test (lambda (obj1 obj2)
             (string= (car obj1) (car obj2))))))

(defun al/bind-keys-from-vars (map-vars &optional key-vars no-default)
  "Bind all keys from KEY-VARS in all maps from MAP-VARS.

MAP-VARS is a variable or a list of variables with keymaps.

KEY-VARS is a variable or a list of variables with bindings.
Each variable should contain a list of key bindings
specifications.  Each spec should be either a cons of a key
string and a function, or a key string (the bound function is nil
in the latter case).

Variables from `al/default-keys-variables' are also used for
binding, unless NO-DEFAULT is non-nil.  The bindings from
KEY-VARS have a priority over the bindings from these variables."
  (declare (indent 1))
  (let* ((key-vars (append (unless no-default al/default-keys-variables)
                           (al/list-maybe key-vars)))
         (specs (al/keys-from-vars key-vars)))
    (al/funcall-or-dolist map-vars
      (lambda (map-var)
        (al/bind-keys-to-map specs map-var)))))

(defun al/clean-map (map-var)
  "Remove all key bindings from MAP-VAR variable with keymap."
  (al/with-check
    :var map-var
    (setcdr (symbol-value map-var) nil)))


;;; Binding buffer local keys

;; Idea from <http://www.emacswiki.org/emacs/BufferLocalKeys>.

(defvar-local al/local-map nil
  "Local keymap used by `al/bind-local-keys-from-vars'.")

(defun al/bind-local-keys-from-vars (&rest vars)
  "Bind all keys from variables VARS locally in the current buffer.
VARS are variables with bindings supported by
`al/bind-keys-from-vars'."
  (setq al/local-map (copy-keymap (current-local-map)))
  (use-local-map al/local-map)
  (al/bind-keys-from-vars 'al/local-map vars t))


;;; Hydra

(with-eval-after-load 'hydra
  (setq hydra-verbose t)
  (al/bind-keys
   :map hydra-base-map
   ("C-4" . hydra--universal-argument)
   ("C-u"))
  (hydra-add-font-lock))


;;; Global keys

(al/bind-keys
 :map ctl-x-map
 ("A"   . al/update-autoloads)
 ("C"   . save-buffers-kill-emacs)
 ("C-8" . insert-char))

(al/bind-keys
 :map universal-argument-map
 ("C-4" . universal-argument-more)
 ("C-u"))

(al/bind-keys
 ("C-4"         . universal-argument)

 ("H-u"         . undo)
 ("H-M-u"       . undo-only)

 ("C-\\"          (toggle-input-method t))
 ("s-7"           (set-input-method nil))
 ("s-8"         . dvorak-russian-computer)
 ("s-9"         . dvorak-qwerty)
 ("s-0"           (set-input-method "greek"))
 ("s-M-7"         (ispell-change-dictionary "en"))
 ("s-M-8"         (ispell-change-dictionary "ru-yeyo"))

 ("<f4>"        . kmacro-end-or-call-macro)
 ("<XF86New>"   . kmacro-end-or-call-macro)
 ("<C-f4>"      . kmacro-start-macro-or-insert-counter)
 ("<C-XF86New>" . kmacro-start-macro-or-insert-counter)
 ("<M-f4>"      . kmacro-edit-macro)
 ("<M-XF86New>" . kmacro-edit-macro)

 ("<f5>"        . compile)
 ("C-="           (describe-char (point)))
 ("C-c r"       . revert-buffer)
 ("C-c p"       . list-processes)
 ("C-c k"       . al/kill-process))

(defalias 'ctl-x-r-prefix ctl-x-r-map)
(al/bind-key "M-R" ctl-x-r-prefix)
(al/bind-keys
 :map ctl-x-r-map
 ("a" . append-to-register)
 ("p" . prepend-to-register))

(defalias 'goto-prefix goto-map)
(al/bind-key "C-M-g" goto-prefix)
(al/bind-keys
 :map goto-map
 ("C-M-g" . goto-line)
 ("c"     . move-to-column)
 ("p"     . goto-char)
 ("h"     . previous-error)
 ("C-M-h" . previous-error)
 ("C-M-n" . next-error))

(al/bind-keys
 :prefix-map al/modes-map
 :prefix-docstring "Map for enabling/disabling modes."
 :prefix "M-M"
 ("M-M" . al/major-mode-to-kill-ring)
 ("a" . artist-mode)
 ("A" . auto-fill-mode)
 ("c" . conf-unix-mode)
 ("e" . emacs-lisp-mode)
 ("f" . font-lock-mode)
 ("o" . org-mode)
 ("p" . python-mode)
 ("P" . paredit-mode)
 ("r" . rainbow-mode)
 ("l" . nlinum-mode)
 ("t" . toggle-truncate-lines)
 ("v" . view-mode)
 ("|" . indent-guide-mode))

;;; keys.el ends here
