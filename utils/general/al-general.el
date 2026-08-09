;;; al-general.el --- Essential functionality for my config files  -*- lexical-binding: t -*-

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
  (require 'cl-lib)
  (require 'fp-utils))
(require 'seq)
(require 'al-places)


;;; `rx' definitions

;; `lisp-mode.el' defines `lisp-mode-symbol' rx name.  However, it is
;; not documented anywhere, so I define `al/lisp-symbol' instead.
(rx-define al/lisp-symbol
  (+ (or (syntax word) (syntax symbol))))

(rx-define al/lisp-symbol-group
  (group al/lisp-symbol))

(rx-define al/space
  (+ (or " " "\t")))

(rx-define al/space*
  (* (or " " "\t")))

(defmacro al/file-regexp (&rest extensions)
  "Return regexp to match file name by EXTENSIONS."
  (declare (indent 0))
  `(rx "." (or ,@extensions) string-end))


;;; Auxiliary code for macros

(defvar al/generate-symbol-counter 0
  "Number used by `al/generate-interned-symbol'.
This variable must be modified only by `al/generate-interned-symbol'.")

(defun al/generate-interned-symbol (&optional prefix)
  "Return a new interned symbol.
This is similar to `gensym' except the returned symbol is interned."
  (intern (format "%s%d" (or prefix "al/generated--")
                  (setq al/generate-symbol-counter
                        (1+ al/generate-symbol-counter)))))

(defmacro al/with-keywords (body variables &rest rest)
  "Auxiliary macro used to define macros with keywords.

The following local variables are available inside REST:

  All symbols from VARIABLES list; these variables have values from
  keywords (with the same names) taken from BODY.

  BODY with keyword pairs removed."
  (declare (indent 2))
  `(let ((body ,body)
         ,@variables)
     (while (keywordp (car body))
       (let ((keyword (pop body))
             (value   (pop body)))
         (cond
          ,@(mapcar (lambda (var)
                      `((eq keyword ,(intern
                                      (concat ":" (symbol-name var))))
                        (setq ,var value)))
                    variables)
          (t
           (al/warning-message "Unknown keyword: %s" keyword)))))
     ,@rest))

(defmacro al/eval-when-compile (&rest body)
  "Evaluate BODY at compile time and do nothing for interpreted code."
  (declare (indent 0) (debug t))
  `(when (bound-and-true-p byte-compile-current-file)
     ,@body))


;;; Simple wrappers for hooks

(defun al/show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(defun al/no-truncate-lines ()
  (setq-local truncate-lines nil))

(defun al/inhibit-field-motion ()
  (setq-local inhibit-field-text-motion t))

(defun al/bar-cursor-type ()
  (setq-local cursor-type 'bar))

(defun al/hbar-cursor-type ()
  (setq-local cursor-type 'hbar))

(defun al/no-syntactic-font-lock ()
  (setq-local font-lock-keywords-only t))

(defun al/set-comment-column ()
  (setq-local comment-column 32))

(defun al/set-default-paragraph ()
  (setq-local paragraph-start    (default-value 'paragraph-start)
              paragraph-separate (default-value 'paragraph-separate)))

(defun al/text-scale+1 ()
  "Increase the font size in the current buffer by 1."
  (text-scale-increase 1))


;;; Function utils

(defun al/funcall (function &rest arguments)
  "Call FUNCTION with ARGUMENTS.
This is the same as `funcall' except it returns nil instead of error if
FUNCTION does not exist."
  (and (al/function? function)
       (apply function arguments)))

(defun al/funcall-or-dolist (val function)
  "Call FUNCTION on VAL if VAL is not a list.
If VAL is a list, call FUNCTION on each element of the list."
  (declare (indent 1))
  (if (listp val)
      (dolist (v val)
        (funcall function v))
    (funcall function val)))


;;; List utils

(defun al/list-maybe (obj)
  "Return OBJ if it is a list, or a list with OBJ otherwise."
  (if (listp obj) obj (list obj)))

(defun al/next-element (list &optional element)
  "Return next element from LIST.
If ELEMENT is nil or not in LIST, return the first element of LIST.
If ELEMENT is an element of LIST, return an element placed after it."
  (if element
      (or (cadr (memq element list))
          (car list))
    (car list)))

(defmacro al/push-new (place newelt &optional testfn)
  "Push NEWELT to PLACE if not already present.
This is similar to `cl-pushnew' but uses `seq' library instead of `cl-lib'."
  `(let ((elt ,newelt))
     (unless (seq-contains-p ,place elt ,testfn)
       (push elt ,place))))

(defun al/push-after (list after elt test)
  "Add ELT to LIST after the first occurrence of AFTER.
AFTER element is checked with TEST predicate.
If AFTER does not exist, insert ELT to the end of LIST.
Return the updated list."
  (cond
   ((null list)
    (list elt))
   ((funcall test (car list) after)
    (cons (car list) (cons elt (cdr list))))
   (t
    (cons (car list) (al/push-after (cdr list) after elt test)))))

(cl-defun al/add-to-list-after (list-var after-element new-element &key test)
  "Add NEW-ELEMENT to LIST-VAR after the first occurrence of AFTER-ELEMENT.
If NEW-ELEMENT already exists in the list, do nothing.
If AFTER-ELEMENT does not exist, insert NEW-ELEMENT to the end of
LIST-VAR.
TEST key is `eq' by default."
  (let ((list (symbol-value list-var))
        (test (or test #'eq)))
    (unless (seq-find (cut test <> new-element)
                      list)
      (set list-var
           (al/push-after list after-element new-element test)))))

(defun al/assq-delete-all (keys alist &optional predicate)
  "Delete from ALIST all elements whose car is one of KEYS.
This is similar to `assq-delete-all', but KEYS can either be a
single key or a list of keys.  KEYS are checked using
PREDICATE (`memq' by default)."
  (let ((keys (al/list-maybe keys)))
    (seq-remove (lambda (assoc)
                    (and (consp assoc)
                         (funcall (or predicate #'memq)
                                  (car assoc)
                                  keys)))
                  alist)))

(defun al/assoc-delete-all (keys alist &optional _predicate)
  "Delete from ALIST all elements whose car is one of KEYS.
This is similar to `assoc-delete-all', but KEYS can either be a
single key or a list of keys.  KEYS are checked using
PREDICATE (`member' by default)."
  (al/assq-delete-all keys alist #'member))

(defun al/every-nth-element1 (n list)
  "Sub-procedure of `al/every-nth-element'."
  (and list
       (cons (car list)
             (al/every-nth-element1 n (nthcdr n list)))))

(defun al/every-nth-element (n list &optional start)
  "Return a list containing every Nth element from LIST.

START is the starting element (0 by default).

N must be a positive integer.  START must be a non-negative integer."
  (al/every-nth-element1 n (if start (nthcdr start list) list)))


;;; Auxiliary messages

(defmacro al/define-message (name string)
  "Define `al/NAME-string' and `al/NAME-message' functions."
  (declare (indent 0) (debug t))
  (let* ((name-str (symbol-name name))
         (str-name (intern (concat "al/" name-str "-string")))
         (msg-name (intern (concat "al/" name-str "-message"))))
    `(progn
       (defun ,str-name (format-string &rest args)
         ,(concat "Return " name-str " string.")
         (apply #'format
                (concat ,string format-string)
                args))

       (defun ,msg-name (format-string &rest args)
         ,(concat "Display " name-str " message.")
         (message (apply #',str-name format-string args))))))

(al/define-message error "ERROR: ")
(al/define-message warning "WARNING: ")
(al/define-message important "XXX ")
(al/define-message title "⏺ ")


;;; Auxiliary predicates

(defun al/p (predicate val &optional message)
  "Return non-nil if PREDICATE returns non-nil on VAL.
Otherwise display warning MESSAGE on VAL and return nil."
  (or (funcall predicate val)
      (progn (and message (al/warning-message message val))
             nil)))

(defun al/every? (predicate vals &optional message)
  "Return non-nil if PREDICATE returns non-nil on each element of VALS.
If VALS is not a list, call PREDICATE on this value."
  (if (and (listp vals)
           (not (functionp vals))) ; to avoid treating "(lambda …)" as list
      (seq-every-p (cut #'al/p predicate <> message)
                   vals)
    (al/p predicate vals message)))

(defun al/function? (object)
  "Non-nil if OBJECT is a function or a list of functions."
  (al/every? #'functionp object
             "Unknown function `%S'."))

(defun al/bound? (object)
  "Non-nil if OBJECT is a bound symbol or a list of bound symbols."
  (al/every? #'boundp object
             "Symbol `%S' is not bound."))

(defun al/file? (object)
  "Non-nil if OBJECT is an existing file or a list of directories."
  (al/every? #'file-exists-p object
             "File `%s' does not exist."))

(defun al/directory? (object)
  "Non-nil if OBJECT is an existing directory or a list of directories."
  (al/every? #'file-directory-p object
             "Directory `%s' does not exist."))


;;; Auxiliary macros

(defmacro al/with-check (&rest body)
  "Call rest of BODY if all checks are passed successfully.

BODY should start with checks (keyword arguments).  The following
keywords are available: `:fun'/`:var'/`:file'/`:dir'.  Each
keyword argument may be an object or a list of objects.  These
objects are checkced to be a proper function / a bound symbol /
an existing file / an existing directory.

Return nil if checks are not passed."
  (declare (indent 0) (debug (name body)))
  (al/with-keywords body
      (fun var file dir)
    `(when (and ,(or (null fun)  `(al/function?  ,fun))
                ,(or (null var)  `(al/bound?     ,var))
                ,(or (null file) `(al/file?      ,file))
                ,(or (null dir)  `(al/directory? ,dir)))
       ,@body)))

(defmacro al/with-demoted-errors (format &rest body)
  "Run BODY and demote any errors to simple messages.

See `with-demoted-errors' for the meaning of arguments.

`with-demoted-errors' supports an obsolete use where FORMAT string can
be missing.  Because of this, (concat ...) cannot be used for FORMAT in
`with-demoted-errors'.  That is why this macro exists."
  (declare (debug t) (indent 1))
  (let ((err (make-symbol "error")))
    `(condition-case ,err
         ,(macroexp-progn body)
       (error (al/error-message ,format ,err) nil))))

(defmacro al/setq-no-warnings (&rest args)
  "Same as `setq' but suppressing free variable compilation warnings."
  (declare (debug setq))
  (let ((vars (al/every-nth-element 2 args)))
    `(with-suppressed-warnings ((free-vars ,@vars))
       (setq ,@args))))

(defmacro al/setq-file (&rest body)
  "Like `setq' but for setting to file name values.
Check each file, and if it exists set the variable accordingly.
Example:

  (al/setq-file v1 \"/foo\"
                v2 \"/tmp\")

v2 will be set, while v1 will not."
  (declare (debug setq))
  (let ((file-var (make-symbol "file")))
    (macroexp-progn
     (mapcar (pcase-lambda (`(,var ,file))
               `(let ((,file-var ,file))
                  (when (file-exists-p ,file-var)
                    (setq ,var ,file-var))))
             (seq-partition body 2)))))

(defmacro al/lambda-lazy (&rest body)
  "Return an anonymous function ignoring its arguments.

On the first call, the function evaluates BODY and returns result.  On
subsequent calls, just the result of the first call is returned without
BODY evaluation.

As usual, BODY can optionally start with docstring.  After that (before
the optional `interactive' clause), the following optional keywords can
be specified:

  `:predicates'     unquoted list of predicates or a single predicate
                    called on the latest result; if any predicate
                    returns nil, body is reevaluated again to update the
                    result."
  (declare (indent 0) (debug t))
  (let ((docstring (and (stringp (car body))
                        (pop body))))
    (al/with-keywords body
        (predicates)
      (let ((interactive (and (equal (car body) '(interactive))
                              (pop body)))
            (called-var  (unless predicates
                           (make-symbol "called?")))
            (val-var     (make-symbol "value")))
        `(let ((,val-var nil)
               ,@(unless predicates
                   `((,called-var nil))))
           (lambda (&rest _)
             ,docstring
             ,interactive
             (if ,(if predicates
                      `(and ,val-var
                            ,@(mapcar (lambda (p) `(funcall #',p ,val-var))
                                      (al/list-maybe predicates)))
                    called-var)
                 ,val-var
               (setq ,@(unless predicates (list called-var t))
                     ,val-var ,(macroexp-progn body)))))))))

(defmacro al/defun-lazy (name &rest body)
  "Define NAME function evaluating BODY once.
See `al/lambda-lazy' for details."
  (declare (indent 1) (debug t))
  `(defalias ',name (al/lambda-lazy ,@body)))

(defmacro al/with-check-point (&rest body)
  "Evaluate BODY.
Return non-nil, if point position is changed after evaluating.
Return nil otherwise."
  (declare (indent 0) (debug t))
  `(let ((pos (point)))
     ,@body
     (/= pos (point))))

(defmacro al/eval-to-kill-ring (&rest body)
  "Evaluate BODY and return its result.
If the result is string or symbol, put it into `kill-ring' and display
it in minibuffer."
  (declare (indent 0) (debug (name body)))
  (let ((res-var     (make-symbol "res"))
        (res-str-var (make-symbol "res-str")))
    `(let* ((,res-var ,(macroexp-progn body))
            (,res-str-var (cond
                           ((stringp ,res-var) ,res-var)
                           ((symbolp ,res-var) (symbol-name ,res-var)))))
       (when ,res-str-var
         (kill-new ,res-str-var)
         (message "%s" ,res-str-var))
       ,res-var)))

(defmacro al/put (properties &rest args)
  "Put symbol PROPERTIES to values.
Each element of ARGS should have (VALUE SYMBOL ...) form.
PROPERTIES can be a list of symbols or a single symbol.
Call (put SYMBOL PROPERTY VALUE) for each PROPERTY and each SYMBOL."
  (declare (indent 1) (debug (name body)))
  (let ((props (al/list-maybe properties))
        (val-var (make-symbol "value")))
    (macroexp-progn
     (mapcar (pcase-lambda (`(,value . ,symbols))
               `(let ((,val-var ,value))
                  ,@(mapcan
                     (lambda (symbol)
                       (mapcar (lambda (prop)
                                 `(put ',symbol ',prop ,val-var))
                               props))
                     symbols)))
             args))))


;;; (Auto)loading utils

(defun al/add-to-load-path-maybe (&rest dirs)
  "Add existing directories from DIRS to `load-path'."
  (dolist (dir dirs)
    (al/with-check
      :dir dir
      (push dir load-path))))

(defun al/load (file)
  "Load FILE.
Return t if FILE is loaded successfully, nil otherwise.
FILE may omit an extension.  See `load' for details."
  (when (stringp file)
    (or (load file 'noerror)
        (progn (al/warning-message "Failed to load `%s'." file)
               nil))))

(defun al/init-load (&rest files)
  "Load FILES from `al/emacs-init-dir'."
  (dolist (file files)
    (al/with-demoted-errors
        (concat "Loading \"" file "\" init file failed: %S")
      (al/load (al/emacs-init-dir-file file)))))

(defmacro al/autoload (file &rest symbols)
  "Autoload (unquoted) SYMBOLS from file as interactive commands."
  (declare (indent 1))
  (macroexp-progn
   (mapcar (lambda (symbol)
             `(autoload ',symbol ,file nil t))
           symbols)))

(defmacro al/require (&rest features)
  "Load FEATURES if not loaded yet.
FEATURES should be unquoted symbols.
Return non-nil if all FEATURES loaded successfully.
Return nil and show warning messages otherwise."
  (declare (indent 0))
  (macroexp-progn
   (mapcar (lambda (feature)
             `(or (require ',feature nil t)
                  (progn
                    (al/warning-message "`%s' feature is not available"
                                        ',feature)
                    nil)))
           features)))

(defvar al/load-paths nil
  "List of `load-path' lists added by `al/load-autoloads'.")

(declare-function al/generate-autoloads "al-autoload")

(defun al/load-autoloads (name directory autoloads-file &rest args)
  "Load AUTOLOADS-FILE, generate it for DIRECTORY if needed.

NAME is a string used for messages.

Additional ARGS are sent to `al/generate-autoloads'.

Push added `load-path' to `al/load-paths'."
  (when (file-exists-p directory)
    (unless (file-exists-p autoloads-file)
      (al/with-demoted-errors
          (concat "Generating " name " autoloads failed: %S")
        (require 'al-autoload)
        (apply #'al/generate-autoloads directory
               :output-file autoloads-file
               args)))
    (al/with-demoted-errors
        (concat "Loading " name " autoloads failed: %S")
      (let ((count (length load-path)))
        (al/load autoloads-file)
        (push (seq-subseq load-path 0 (- count))
              al/load-paths)))))


;;; Hook and "after load" functionality

;; My naming rule:
;;
;; - `al/call-*' is a macro calling FUNCTIONS when some event occurs;
;;
;; - `al/eval-*' is a macro evaluating BODY when some event occurs.

(defmacro al/eval-at-hook (hooks &rest body)
  "Add function evaluating BODY to all HOOKS.

HOOKS should be an unquoted symbol (hook variable) or list of those.

BODY can start with the following optional keywords:

  `:name'       name of the generated function;

  `:depth',`:local'
                additional arguments passed to `add-hook'."
  (declare (indent 1))
  (al/with-keywords body
      (name depth local)
    (let* ((single-hook? (symbolp hooks))
           (fun-expr `(lambda (&rest _) ,@body))
           (fun      (if name `',name fun-expr))
           (fun-var  (and (not name)
                          (not single-hook?)
                          (make-symbol "fun")))
           (exprs    (if single-hook?
                         `((add-hook ',hooks ,fun ,depth ,local))
                       (mapcar (lambda (hook)
                                 `(add-hook ',hook ,(or fun-var fun)
                                            ,depth ,local))
                               hooks))))
      (cond
       (fun-var
        `(let ((,fun-var ,fun)) ,@exprs))
       (name
        `(progn
           (defalias ,fun ,fun-expr)
           ,@exprs))
       (t (car exprs))))))

(defmacro al/call-at-hook (hooks &rest functions)
  "Call all FUNCTIONS in all HOOKS.

Both HOOKS and FUNCTIONS should be unquoted symbols or lists of those.
Each function will be called using `al/funcall', so non-existing
functions are safe to add.

FUNCTIONS can optionally start with keywords supported by
`al/eval-at-hook'."
  (declare (indent 1))
  (let ((kw-args '()))
    (while (keywordp (car functions))
      (setq kw-args
            (append (list (pop functions)
                          (pop functions))
                    kw-args)))
    `(al/eval-at-hook ,hooks
       ,@kw-args
       ,@(mapcar (lambda (f) `(al/funcall ',f))
                 functions))))

(defmacro al/eval-after-init (&rest body)
  "Evaluate BODY after Emacs init.
BODY can optionally start with keywords supported by `al/eval-at-hook'."
  (declare (indent 0))
  `(al/eval-at-hook after-init-hook ,@body))

(defmacro al/call-after-init (&rest functions)
  "Call FUNCTIONS after Emacs init.

FUNCTIONS should be unquoted symbols, they will be called using
`al/funcall'.

FUNCTIONS can optionally start with keywords supported by
`al/eval-at-hook'."
  (declare (indent 0))
  `(al/call-at-hook after-init-hook ,@functions))

(defmacro al/eval-after-frame-init (&rest body)
  "Evaluate BODY after frame start.

BODY can start with the following optional keywords:

  `:terminal'   can be `graphical' to evaluate BODY only for a graphical
                frame, `text' to evaluate BODY for a text-only terminal,
                or `any' (default) to evaluate BODY for any terminal.

  `:once'       can be `nil' (default) meaning BODY is evaluated for
                every new frame, or `t' to evaluate BODY only once after
                starting the first frame.

This macro exists because standalone Emacs and Emacs started as a daemon
start frames differently.  Also not all settings are possible/desired on
a non-graphical terminal."
  (declare (indent 0))
  (al/with-keywords body
      (name terminal once)
    (let ((name (or name (al/generate-interned-symbol "al/frame-init-"))))
      `(progn
         ,(and once `(defvar ,name nil))
         (defun ,name ()
           (when (and ,(or (null once)
                           `(null ,name))
                      ,(cond
                        ((eq terminal 'graphical)
                         '(display-graphic-p))
                        ((eq terminal 'text)
                         '(null (display-graphic-p)))
                        (t t)))
             ,@body
             ,(and once `(setq ,name t))))
         (add-hook (if (daemonp)
                       'server-after-make-frame-hook
                     'after-init-hook)
                   ',name)))))

(defmacro al/eval-after-load (feature &rest body)
  "Execute BODY after FEATURE load.

This is similar to `with-eval-after-load' except it does not produce
unneeded compilation warnings at compile time.

FEATURE should be an unquoted symbol.

BODY can start with the following optional keywords:

  `:load'       can be `nil' (default) to do nothing additionally, `t'
                to load FEATURE immediately, or anything else to load
                FEATURE at `after-init-hook'."
  (declare (indent 1) (debug (form def-body)))
  (al/with-keywords body
      (load no-warning)
    (al/eval-when-compile
      (unless (or no-warning (require feature nil t))
        (al/warning-message "`%s' feature is not available" feature)))
    (cond
     ((null load)
      `(eval-after-load ',feature (lambda () ,@body)))
     ((eq t load)
      `(when (al/require ,feature)
         ,@body))
     (t
      `(progn
         (eval-after-load ',feature (lambda () ,@body))
         (al/eval-after-init (al/require ,feature)))))))


;;; Command utils

(defmacro al/define-multi-command (name &rest functions)
  "Define NAME interactive command.
This command will execute FUNCTIONS in order until one of them returns
non-nil value."
  (declare (indent 1) (debug t))
  (let* ((name-str        (symbol-name name))
         (subfun-name-str (concat name-str "-1"))
         (subfun-name     (intern subfun-name-str))
         (var-name-str    (concat name-str "-functions"))
         (var-name        (intern var-name-str)))
    `(progn
       (defvar ,var-name '(,@functions)
         ,(concat "List of functions for `" name-str "'.
Each element should be a function called without arguments.  If it
returns nil, the next function is called, and so on until the end of
this list or until success i.e., until one of the functions returns
non-nil."))

       (defun ,subfun-name (funs)
         ,(concat "Sub-function for `" name-str "'.")
         (when funs
           (or (funcall      (car funs))
               (,subfun-name (cdr funs)))))

       (defun ,name ()
         ,(concat "Execute `" var-name-str "' until success.")
         (interactive)
         (,subfun-name ,var-name)))))


;;; Syntax table utils

(defmacro al/modify-syntax (table-name &rest specs)
  "Update syntax table according to SPECS.
TABLE-NAME is a name (unquoted symbol) of a syntax table variable.
SPECS are (CHAR NEWENTRY) elements.  See `modify-syntax-entry'
for details."
  (declare (indent 1))
  (let ((table-var (make-symbol "table")))
    `(al/with-check
       :var ',table-name
       (let ((,table-var (symbol-value ',table-name)))
         ,@(mapcar (pcase-lambda (`(,char ,entry))
                     `(modify-syntax-entry ,char ,entry ,table-var))
                   specs)))))

(defmacro al/modify-page-break-syntax (table-name)
  "Set non-whitespace syntax for ^L in syntax table TABLE-NAME.
Page break should not belong to whitespace syntax, because
`back-to-indentation' moves the point after ^L character which is not good.
Also it (default syntax) breaks `indent-guide-mode'."
  `(al/modify-syntax ,table-name (?\f ">   ")))


;;; Miscellaneous utils

(defun al/intern (string-or-symbol)
  "Like `intern' except STRING-OR-SYMBOL can also be a symbol."
  (if (symbolp string-or-symbol)
      string-or-symbol
    (intern string-or-symbol)))

(provide 'al-general)

;;; al-general.el ends here
