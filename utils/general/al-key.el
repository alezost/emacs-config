;;; al-key.el --- Additional functionality for working with key bindings  -*- lexical-binding: t -*-

;; Copyright © 2013–2026 Alex Kost

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
(require 'seq)
(require 'al-general)
(require 'al-list)


;;; Binding keys

;; `al/bind-keys' macro and related code originates from `bind-key'
;; package: <https://github.com/jwiegley/use-package>.

(defvar al/override-global-map (make-keymap)
  "Keymap with key bindings to take precedence over other keymaps.")

(define-minor-mode al/override-global-mode
  "Minor mode with key bindings to override other modes."
  :init-value t)

(al/push-new emulation-mode-map-alists
             `((al/override-global-mode . ,al/override-global-map)))

(defun al/key-command (cmd-spec)
  "Return command value for `al/bind-key' macro."
  (cond ((null cmd-spec) nil)
        ((listp cmd-spec)
         (if (eq (car cmd-spec) 'lambda)
             cmd-spec
           `(lambda () (interactive) ,@cmd-spec)))
        ((stringp cmd-spec)
         (key-parse cmd-spec))
        ((and (symbolp cmd-spec)
              (not (commandp cmd-spec))
              (boundp cmd-spec))
         cmd-spec)
        (t `',cmd-spec)))

(defmacro al/bind-key (key-name command &optional keymap)
  "Bind KEY-NAME to COMMAND in KEYMAP.

KEY-NAME should be a string or a vector taken by `define-key'.

COMMAND may be either:

  - nil (to unbind the key if it is already bound in KEYMAP),

  - an unquoted symbol, which is either a command or a variable with
    keymap,

  - or a list (it will be wrapped into interactive `lambda' form).

If KEYMAP is not specified, use `global-map'.

Examples:

  (al/bind-key \"C-f\" nil)
  (al/bind-key \"C--\" \"–\" key-translation-map)
  (al/bind-key \"C-j\" newline lisp-mode-map)
  (al/bind-key [return] newline-and-indent lisp-mode-shared-map)
  (al/bind-key \"C-s-b\" ((backward-word) (backward-char)))"
  (declare (indent 1))
  (let ((cmd (al/key-command command))
        (key (if (stringp key-name)
                 (key-parse key-name)
               key-name))
        (map (or keymap 'global-map)))
    (if command
        `(define-key ,map ,key ,cmd)
      ;; Bind key to nil only if it already exists in MAP.  Otherwise,
      ;; global keymap will be used skipping all the intermediate maps.
      ;; For example, if "C-e" is bound in `icomplete-minibuffer-map'
      ;; and we bind "C-e" to nil in `minibuffer-local-map', then "C-e"
      ;; from the global keymap will be used, not from
      ;; `icomplete-minibuffer-map'.
      `(if (lookup-key ,map ,key)
           (define-key ,map ,key nil)))))

(defmacro al/bind-key* (key-name command)
  (declare (indent 1))
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
  (declare (indent 0))
  (al/with-keywords args
      (map prefix prefix-map prefix-docstring)
    (if (or (and prefix (not prefix-map))
            (and (not prefix) prefix-map))
        (al/error-message
         "Both, :prefix (%s) and :prefix-map (%s), must be specified"
         prefix prefix-map)
      (let ((body
             `(,@(when prefix-map
                   `((defvar ,prefix-map)
                     ,(when prefix-docstring
                        `(put ',prefix-map 'variable-documentation
                              ,prefix-docstring))
                     (define-prefix-command ',prefix-map)
                     (al/bind-key ,prefix ,prefix-map ,map)))
               ,@(mapcar (lambda (binding)
                           (pcase (al/list-maybe binding)
                             (`(,key . ,command)
                              `(al/bind-key ,key ,command
                                            ,(or prefix-map map)))))
                         %body))))
        (if map
            `(al/with-check
               :var ',map
               ,@body)
          (macroexp-progn body))))))

(defmacro al/bind-keys* (&rest args)
  (declare (indent 0))
  `(al/bind-keys :map al/override-global-map ,@args))


;;; Binding keys from maps

(defvar al/default-keys-variables nil
  "Default list of variables used by `al/bind-keys-from-vars'.")

(defun al/bind-keys-to-map (key-specs &optional map-var)
  "Bind all keys from KEY-SPECS in MAP-VAR.
KEY-SPECS is an alist of keybinding strings and functions (the
same as the rest of arguments taken by `al/bind-keys').
MAP-VAR is a variable with keymap. If it is nil, use `global-map'."
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
  (let* ((vars (seq-filter #'al/bound? vars))
         ;; Reverse vars to make `seq-uniq' remove duplicates from the
         ;; first vars, not from the last ones.
         (vars (nreverse vars))
         (keys-raw (apply #'append
                          (mapcar #'symbol-value vars)))
         (keys (mapcar #'al/list-maybe keys-raw)))
    (seq-uniq
     keys
     (lambda (obj1 obj2)
       (equal (car obj1) (car obj2))))))

(defun al/bind-keys-from-vars (map-vars &optional key-vars no-default)
  "Bind all keys from KEY-VARS in all maps from MAP-VARS.

MAP-VARS is a variable or a list of variables with keymaps.
If MAP-VARS is nil, use `global-map' and set NO-DEFAULT to t.

KEY-VARS is a variable or a list of variables with bindings.
Each variable should contain a list of key bindings specifications.
Each spec should have either (KEY-NAME . COMMAND) or KEY-NAME form.
See `al/bind-key' for the meaning of KEY-NAME and COMMAND.

Variables from `al/default-keys-variables' are also used for
binding, unless NO-DEFAULT is non-nil.  The bindings from
KEY-VARS have a priority over the bindings from these variables."
  (declare (indent 1))
  (let* ((key-vars (append (and map-vars
                                (null no-default)
                                al/default-keys-variables)
                           (al/list-maybe key-vars)))
         (specs (al/keys-from-vars key-vars)))
    (if map-vars
        (al/funcall-or-dolist map-vars
          (lambda (map-var)
            (al/bind-keys-to-map specs map-var)))
      (al/bind-keys-to-map specs))))


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


;;; Misc

(defun al/clean-map (map-var)
  "Remove all key bindings from MAP-VAR variable with keymap."
  (al/with-check
    :var map-var
    (setcdr (symbol-value map-var) nil)))

(provide 'al-key)

;;; al-key.el ends here
