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

;;; Commentary:

;; Some macros from this package expand to functions from `al-general'
;; package, so if you use it at compilation time, along with
;;
;;   (eval-when-compile (require 'al-aux-macros))
;;
;; you may also need to use
;;
;;   (require 'al-general)

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
  (and cmd-spec
       (let ((spec (if (listp cmd-spec)
                       (car cmd-spec)
                     cmd-spec)))
         (pcase spec
           ((pred stringp) (key-parse spec))
           ((pred characterp) (vector spec))
           ((pred symbolp)
            (if (and (boundp spec)
                     (not (commandp spec)))
                spec
              `',spec))
           (_ `(lambda () (interactive) ,@cmd-spec))))))

(defmacro al/bind-key (key-name command &optional keymap)
  "Bind KEY-NAME to COMMAND in KEYMAP.

KEY-NAME should be a string or a vector taken by `define-key'.

COMMAND may be either:

  - nil to unbind the key if it is already bound in KEYMAP;

  - a character or a string;

  - an unquoted symbol, which is either a command or a variable with
    keymap;

  - or a list (it will be wrapped into interactive `lambda' form).

If KEYMAP is not specified, use `global-map'.

Examples:

  (al/bind-key \"C-f\" nil)
  (al/bind-key \"C--\" ?– key-translation-map)
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

  `:map'        keymap into which the key bindings should be added;

  `:create'     if non-nil, create `:map' variable;

  `:check'      if non-nil, check if `:map' variable exists;

  `:clean'      if non-nil, remove all bindings from `:map' before
                adding the new ones;

  `:prefix-map' name of a prefix map that should be created for
                these bindings;

  `:prefix-key' prefix key for these bindings;

  `:prefix-doc' docstring of the prefix map variable.

The rest ARGS may have one of the following forms:

  KEY-NAME      to unbind this key;

  (KEY-NAME CMD-SPEC) or
  (KEY-NAME CMD-SPEC ...)
                to bind KEY-NAME to CMD-SPEC

See `al/bind-key' for details."
  (declare (indent 0))
  (al/with-keywords args
      ( map check create clean
        prefix-key prefix-map prefix-doc )
    (if (or (and prefix-key (not prefix-map))
            (and (not prefix-key) prefix-map))
        (al/error-message
         "Both, :prefix-key (%s) and :prefix-map (%s), must be specified"
         prefix-key prefix-map)
      (let ((body
             `(,@(and map create
                      `((defvar ,map (make-sparse-keymap))))
               ,@(and map clean
                      `((al/clean-keymap ,map)))
               ,@(when prefix-map
                   `((defvar ,prefix-map)
                     ,(when prefix-doc
                        `(put ',prefix-map 'variable-documentation
                              ,prefix-doc))
                     (define-prefix-command ',prefix-map)
                     (al/bind-key ,prefix-key ,prefix-map ,map)))
               ,@(when %body
                   ;; Here, we just bind some keys to some commands.
                   ;; Warnings about undefined functions are the only
                   ;; compilation warnings that we can get here.
                   `((with-no-warnings
                       ,@(mapcar (lambda (binding)
                                   (pcase (al/list-maybe binding)
                                     (`(,key . ,command)
                                      `(al/bind-key ,key ,command
                                                    ,(or prefix-map map)))))
                                 %body)))))))
        (if (and map check)
            `(al/with-check
               :var ',map
               ,@body)
          (macroexp-progn body))))))

(defmacro al/bind-keys* (&rest args)
  (declare (indent 0))
  `(al/bind-keys :map al/override-global-map ,@args))

(defmacro al/bind-digits (&rest args)
  "Bind digit keys (0, 1, 2, ...) to some commands.

ARGS can start with the following optional keywords:

  `:start-from' integer to start counting from (default is zero);

  any other keyword supported by `al/bind-keys'.

The rest ARGS have (PREFIX STRING) or (PREFIX COMMANDS ...) form, where:

  PREFIX        is a string prepended to digit, can be a key
                modifier (e.g. \"M-\"), a prefix key (e.g., \"k\"), or
                nil for no prefixes (only digit keys are bound);

  STRING        characters that digits should be bound to;

  COMMANDS      symbols (command names) or other command
                specifications supported by `al/bind-key'.

Example to make \"C-x 8 <N>\" insert superscript digits and
to make \"C-x 8 C-<N>\" insert subscript digits:

  (al/bind-digits
    :map iso-transl-ctl-x-8-map
    (nil \"⁰¹²³⁴⁵⁶⁷⁸⁹\")
    (\"C-\" \"₀₁₂₃₄₅₆₇₈₉\"))"
  (declare (indent 0))
  (al/with-keywords args
      (start-from)
    :allow-other-keys t
    (let ((start (or start-from 0)))
      `(al/bind-keys
         ,@%other-keys
         ,@(mapcan (pcase-lambda (`(,prefix . ,rest))
                     (seq-map-indexed
                      (lambda (cmd n)
                        (list (concat prefix
                                      (number-to-string (+ start n)))
                              cmd))
                      (if (stringp (car rest))
                          (car rest)
                        rest)))
                   %body)))))


;;; Translating keys

(defvar al/self-insert-commands
  '(undefined
    self-insert-command
    org-self-insert-command
    isearch-printing-char)
  "List of self-inserting commands.")

(defun al/key-if-bound (key &optional fallback)
  "Return KEY if it is bound in the currently active keymaps.

Return FALLBACK, if KEY is not bound or bound to one of
`al/self-insert-commands' commands.

This is a helper function called by functions generated by
`al/translate-keys' macro."
  ;; We cannot simply check if KEY is bound because KEY is not the whole
  ;; key sequence, it is always a vector with the last key of the
  ;; sequence.  For example, assume "e" is translated to "↓", and "↓"
  ;; is bound in the current major mode.  Then, when "C-h e" is pressed,
  ;; and only KEY is checked with `key-binding', Emacs will tell "C-h ↓
  ;; is undefined".  That's why we need to check the whole key sequence
  ;; ("C-h ↓" instead of just "↓") by replacing the last key from the
  ;; current sequence with KEY.
  (let ((key-seq (this-single-command-keys)))
    (aset key-seq
          (1- (length key-seq))
          (aref key 0))
    (let ((binding (key-binding key-seq)))
      (if (and binding
               (not (memq binding al/self-insert-commands)))
          key
        fallback))))

(defun al/translate-keys-1 (from-mod to-mod from-char to-char)
  "Helper for `al/translate-keys'."
  (let ((from-key (key-parse (concat from-mod (string from-char))))
        (to-key   (key-parse (concat to-mod   (string to-char)))))
    (let ((fun-name (intern (format "al/translate-character-%s%d"
                                    from-mod from-char))))
      `((defun ,fun-name (&rest _)
          (al/key-if-bound ,to-key ,from-key))
        (define-key key-translation-map ,from-key ',fun-name)))))

(defmacro al/translate-keys (modifiers &rest bindings)
  "Bind characters at `key-translation-map'.

For each modifier and each character pair from BINDINGS, a new key
translation will be defined in `key-translation-map'.

Each modifier from MODIFIERS list is either:

  a string with modifier prefix (like \"C-\" or \"C-M-\") or

  (FROM-MOD TO-MOD) list, where FROM-MOD is a modifier of FROM-CHAR and
  TO-MOD is a modifier of TO-CHAR (see below).

A modifier can be an empty string which means a key should be bound
without any modifier.

BINDINGS is a list of (FROM-CHAR TO-CHAR) lists, where FROM-CHAR is a
character to bind and TO-CHAR is the respecting translated character.

Examples:

  (al/translate-keys (\"C-\" \"M-\")
    (?p ?↑))

maps \"C-p\" to \"C-↑\" and \"M-p\" to \"M-↑\", so that when you press
\"C-p\" Emacs will tell you \"C-↑\" is undefined.  Thus you can use
\"C-↑\" and \"M-↑\" in your keybindings.

  (al/translate-keys ((\"\" \"S-\"))
    (?< ?↤)
    (?> ?↦))

maps \"<\" to \"S-↤\" and \">\" to \"S-↦\"."
  (declare (indent 1))
  (macroexp-progn
   (mapcan (pcase-lambda (`(,from-char ,to-char))
             (mapcan (lambda (mod)
                       (pcase mod
                         (`(,from-mod ,to-mod)
                          (al/translate-keys-1 from-mod to-mod
                                               from-char to-char))
                         (_
                          (al/translate-keys-1 mod mod
                                               from-char to-char))))
                     modifiers))
           bindings)))


;;; Binding keys from variables

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

(defun al/clean-keymap (keymap &optional clean-parent)
  "Remove all key bindings from KEYMAP.
If CLEAN-PARENT is non-nil, also remove the parent keymap."
  (if clean-parent
      (setcdr keymap nil)
    (let ((parent (keymap-parent keymap)))
      (setcdr keymap nil)
      (set-keymap-parent keymap parent))))

(provide 'al-key)

;;; al-key.el ends here
