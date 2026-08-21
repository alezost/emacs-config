;;; al-aux-macros.el --- Auxiliary macros  -*- lexical-binding: t -*-

;; Copyright © 2016–2026 Alex Kost

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

;; This file contains auxiliary macros intended to be used at
;; compilation time.  Some of these macros expand to functions from
;; `al-general' package, so along with
;;
;;   (eval-when-compile (require 'al-aux-macros))
;;
;; you may also need to use
;;
;;   (require 'al-general)

;;; Code:

(require 'seq)
(require 'al-list)

(defmacro al/file-regexp (&rest extensions)
  "Return regexp to match file name by EXTENSIONS."
  (declare (indent 0))
  `(rx "." (or ,@extensions) string-end))


;;; Auxiliary code for macros

(defmacro al/with-keywords-1 (body variables allow-other-keys &rest rest)
  "Helper macro for `al/with-keywords'."
  (declare (indent 2))
  `(let ((%body ,body)
         ,@(and allow-other-keys '(%other-keys))
         ,@variables)
     (while (keywordp (car %body))
       (let ((keyword (pop %body))
             (value   (pop %body)))
         (cond
          ,@(mapcar (lambda (var)
                      `((eq keyword ,(intern
                                      (concat ":" (symbol-name var))))
                        (setq ,var value)))
                    variables)
          (t
           ,(if allow-other-keys
                '(setq %other-keys (append (list keyword value)
                                           %other-keys))
              '(message "WARNING Unknown keyword: `%s'." keyword))))))
     ,@rest))

(defmacro al/with-keywords (body variables &rest rest)
  "Auxiliary macro used to define macros with keyword arguments.

REST can start with the following optional keywords:

  `:allow-other-keys'   if non-nil, add `%other-keys' symbol to
                        VARIABLES and do not print warning messages for
                        BODY keywords that are not in the VARIABLES
                        list.

The following local variables are available inside REST:

  All symbols from VARIABLES list; these variables have values from
  keywords (with the same names) taken from BODY.

  `%other-keys'         if `:allow-other-keys' was specified, this
                        variable has (KEY VALUE ...) value for all
                        keywords that are not in VARIABLES.

  `%body'               BODY with all keyword pairs removed."
  (declare (indent 2) (debug (form (&rest symbolp) body)))
  (al/with-keywords-1 rest
      (allow-other-keys)
    nil
    `(al/with-keywords-1 ,body
         ,variables
       ,allow-other-keys
       ,@%body)))

(defmacro al/eval-when-compile (&rest body)
  "Evaluate BODY at compile time and do nothing for interpreted code."
  (declare (indent 0) (debug t))
  `(if (bound-and-true-p byte-compile-current-file)
       ,(macroexp-progn body)))


;;; Auxiliary macros

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
       (error (al/error-message ,format ,err)
              nil))))

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
      (let ((interactive (and (equal (car %body) '(interactive))
                              (pop %body)))
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
                     ,val-var ,(macroexp-progn %body)))))))))

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

  `:once'       if non-nil, evaluate BODY only once during the first
                hook call;

  `:eval-hook'  if non-nil, use HOOKS verbatim i.e., evaluate HOOKS
                expression during `add-hook' call instead of
                considering it a list of hook variables;

  `:depth', `:local'
                additional arguments passed to `add-hook'."
  (declare (indent 1))
  (al/with-keywords body
      (name once eval-hook depth local)
    (let* ((single-hook? (or eval-hook (symbolp hooks)))
           (fun-expr (if once
                         `(al/lambda-lazy ,@%body)
                       `(lambda (&rest _) ,@%body)))
           (fun      (if name `',name fun-expr))
           (fun-var  (and (not name)
                          (not single-hook?)
                          (make-symbol "fun")))
           (exprs    (if single-hook?
                         `((add-hook ,(if eval-hook hooks `',hooks)
                                     ,fun ,depth ,local))
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
                or `any' (default) to evaluate BODY for any terminal;

  any other keyword supported by `al/eval-at-hook'.

This macro exists because standalone Emacs and Emacs started as a daemon
start frames differently.  Also not all settings are possible/desired on
a non-graphical terminal."
  (declare (indent 0))
  (al/with-keywords body
      (terminal)
    :allow-other-keys t
    `(al/eval-at-hook (if (daemonp)
                          'server-after-make-frame-hook
                        'after-init-hook)
       :eval-hook t
       ,@%other-keys
       (if ,(cond ((eq terminal 'graphical)
                   '(display-graphic-p))
                  ((eq terminal 'text)
                   '(not (display-graphic-p)))
                  (t t))
           ,(macroexp-progn %body)))))

(defmacro al/eval-after-load (feature &rest body)
  "Execute BODY after FEATURE load.

This is similar to `with-eval-after-load' except it does not produce
unneeded compilation warnings at compile time.

FEATURE should be an unquoted symbol.

BODY can start with the following optional keywords:

  `:no-warning' if non-nil, do not show warning message when FEATURE is
                not available; actually, FEATURE is not loaded at
                compile time at all;

  `:load'       can be `nil' (default) to do nothing additionally, `t'
                to load FEATURE immediately, or anything else to load
                FEATURE at `after-init-hook'."
  (declare (indent 1) (debug (form def-body)))
  (al/with-keywords body
      (load no-warning)
    (or no-warning
        (al/eval-when-compile
          (unless (require feature nil t)
            (message "WARNING: `%s' feature is not available." feature))))
    (cond
     ((null load)
      `(eval-after-load ',feature (lambda () ,@%body)))
     ((eq t load)
      `(if (al/require ,feature)
           ,(macroexp-progn body)))
     (t
      `(progn
         (eval-after-load ',feature (lambda () ,@%body))
         (al/eval-after-init (al/require ,feature)))))))

(defmacro al/eval-settings-after-load (&rest args)
  "Load settings for a package after it is loaded.

ARGS is a list of (FEATURE NAME) or (FEATURE BODY ...) values, where

  FEATURE       is a package feature symbol;

  NAME          is a file name loaded with `al/load-settings' after
                FEATURE is required;

  BODY          is a list of lisp expressions to evaluate after
                FEATURE is required."
  (declare (indent 0))
  (macroexp-progn
   (mapcar (pcase-lambda (`(,feature . ,body))
             (if (stringp (car body))
                 `(al/eval-after-load ,feature
                    :no-warning t
                    (al/load-settings ,(car body)))
               `(al/eval-after-load ,feature
                  ,@body)))
           args)))


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
  (macroexp-progn
   (mapcar (pcase-lambda (`(,char ,entry))
             `(modify-syntax-entry ,char ,entry ,table-name))
           specs)))

(defmacro al/modify-page-break-syntax (table-name)
  "Set non-whitespace syntax for ^L in syntax table TABLE-NAME.
Page break should not belong to whitespace syntax, because
`back-to-indentation' moves the point after ^L character which is not good.
Also it (default syntax) breaks `indent-guide-mode'."
  `(al/modify-syntax ,table-name (?\f ">   ")))

(al/put doc-string-elt
  (1 al/lambda-lazy)
  (2 al/defun-lazy))

(provide 'al-aux-macros)

;;; al-aux-macros.el ends here
