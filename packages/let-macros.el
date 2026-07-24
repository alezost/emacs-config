;;; let-macros.el --- Additional `let'-like macros  -*- lexical-binding: t -*-

;; Copyright © 2026 Alex Kost

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

;; This file provides the following macros:
;;
;; - `if-let-',
;; - `if-let+' and its alias `if-let',
;; - `if-let1',
;; - `if-letn',
;; - `when-let-' and its alias `let-',
;; - `when-let+' and its alias `when-let',
;; - `when-let1',
;; - `when-letn'.
;;
;; Along with usual (NAME EXPRESSION) bindings, all these macros accept
;; (NAME EXPRESSION CLAUSES ...) forms.  See `if-let-' for details.
;;
;; `*-let1' / `*-letn' are somewhat analogous to `prog1' / `progn'.
;; That's why they have these names.

;;; Code:

(require 'fp-utils)

(defmacro if-let--compose-funcall (value &rest functions)
  "Call composed FUNCTIONS on VALUE.
More precisely, (if-let--compose-funcall VALUE #\\='F1 #\\='F2 ... #\\='Fn)
expands to (Fn ... (F2 (F1 VALUE)))."
  (declare (indent 1) (debug t))
  (if functions
      `(if-let--compose-funcall (funcall ,(car functions) ,value)
         ,@(cdr functions))
    value))

(defmacro if-let- (bindings then &rest else)
  "Augmented `let'-like macro.

It has the following form:

  (if-let- ((NAME EXPRESSION [CLAUSES ...])
            ...)
      THEN
    ELSE ...)

If CLAUSES are not specified, then `if-let-' is equivalent to `let*'
with only THEN clause.  ELSE is never evaluated in this case.

Each clause from CLAUSES should have one of the following forms:

  (=> FUNCTIONS ...)

    FUNCTIONS are composed from left to right (i.e., the leftmost
    function is called the first) and called on NAME variable.  The
    result is taken as the new value for NAME.

  (<= FUNCTIONS ...)

    each function from FUNCTIONS is called with NAME variable as a
    single argument.  If any of them returns nil, evaluate ELSE.
    If all checks in all \"<=\" clauses pass, evaluate THEN.

See example for `when-let-'."
  (declare (indent 2)
           (debug ((&rest (symbolp form &rest sexp))
                   body)))
  (pcase bindings
    ('()
     then)
    (`((,var ,expr . ,clauses)
       . ,rest-bindings)
     (pcase clauses
       ('()
        `(let ((,var ,expr))
           (if-let- ,rest-bindings ,then ,@else)))
       (`((<= . ,functions) . ,rest-clauses)
        `(let ((,var ,expr))
           (if (and<= ,var ,@functions)
               (if-let- (,@(and rest-clauses
                                `((,var ,var ,@rest-clauses)))
                         ,@rest-bindings)
                   ,then
                 ,@else)
             ,@else)))
       (`((=> . ,functions) . ,rest-clauses)
        `(if-let- ((,var (if-let--compose-funcall ,expr ,@functions)
                         ,@rest-clauses)
                   ,@rest-bindings)
             ,then
           ,@else))))))

(defmacro if-let+ (bindings then &rest else)
  "Usual `if-let' construct with optional auxiliary CLAUSES.

It has the following form:

  (if-let+ ((NAME EXPRESSION [CLAUSES ...])
            ...)
      THEN
    ELSE ...)

See `if-let-' for the meaning of CLAUSES.

If any NAME variable is nil or if any of \"<=\" or \"=>\" CLAUSES
returns nil, evaluate ELSE.  Otherwise (if all NAME variables pass all
checks), evaluate THEN."
  (declare (indent 2) (debug if-let-))
  (pcase bindings
    ('()
     then)
    (`((,var ,expr . ,clauses)
       . ,rest-bindings)
     (pcase clauses
       ('()
        `(let ((,var ,expr))
           (if ,var
               (if-let+ ,rest-bindings ,then ,@else)
             ,@else)))
       (`((<= . ,functions) . ,rest-clauses)
        `(let ((,var ,expr))
           (if (and<= ,var ,@functions)
               (if-let+ (,@(and rest-clauses
                                `((,var ,var ,@rest-clauses)))
                         ,@rest-bindings)
                   ,then
                 ,@else)
             ,@else)))
       (`((=> . ,functions) . ,rest-clauses)
        `(if-let+ ((,var (and=> ,expr ,@functions)
                         ,@rest-clauses)
                   ,@rest-bindings)
             ,then
           ,@else))))))

(defmacro if-let1 (bindings then &rest else)
  "Call `if-let+' on the first binding and `if-let-' on the rest.

For example,

  (if-let1 ((a 1)
            (b 2)
            (c 3))
      (+ a b c)
    0)

expands to

  (if-let+ ((a 1))
      (if-let- ((b 2)
                (c 3))
          (+ a b c)
        0)
    0)"
  (declare (indent 2) (debug if-let-))
  `(if-let+ (,(car bindings))
       (if-let- ,(cdr bindings)
           ,then
         ,@else)
     ,@else))

(defmacro if-letn (bindings then &rest else)
  "Call `if-let+' on the last binding and `if-let-' on the rest.

For example,

  (if-letn ((a 1)
            (b 2)
            (c 3))
      (+ a b c)
    0)

expands to

  (if-let- ((a 1)
            (b 2))
      (if-let+ ((c 3))
          (+ a b c)
        0)
    0)"
  (declare (indent 2) (debug if-let-))
  (let* ((reversed (nreverse bindings))
         (last (car reversed))
         (all-but-last (nreverse (cdr reversed))))
    `(if-let- ,all-but-last
         (if-let+ (,last)
             ,then
           ,@else)
       ,@else)))

(defmacro when-let- (bindings &rest body)
  "Usual `when'-like wrapper for `if-let-'.

It has the following form:

  (when-let- ((NAME EXPRESSION [CLAUSES ...])
              ...)
    BODY ...)

If CLAUSES are not specified, then `when-let-' is equivalent to `let*'.
See `if-let-' for the meaning of CLAUSES.

Example:

  (when-let- ((file \"/tmp/foo\"
                    (<= #\\='file-symlink-p)
                    (=> #\\='file-truename #\\='file-name-nondirectory)
                    (<= (lambda (f) (string-match-p \"\\`emacs\" f)))))
    file)

Here, we check if FILE exists and is a symlink, and if so, we take file
name (without directory) of its true name (following the symlink).
Finally, we check if the file name starts with \"emacs\".  If any check
is not passed, nil is returned.  So the above `when-let-' clause expands
to something like this:

  (let ((file (and<= \"/tmp/foo\" #\\='file-symlink-p)))
    (if file
        (let ((file (and<= (file-name-nondirectory (file-truename val))
                           (lambda (f) (string-match-p \"\\`emacs\" f)))))
          file)))"
  (declare (indent 1) (debug if-let-))
  (list 'if-let- bindings (macroexp-progn body)))

(defmacro when-let+ (bindings &rest body)
  "Usual `when'-like wrapper for `if-let+'."
  (declare (indent 1) (debug if-let-))
  (list 'if-let+ bindings (macroexp-progn body)))

(defmacro when-let1 (bindings &rest body)
  "Usual `when'-like wrapper for `if-let1'."
  (declare (indent 1) (debug if-let-))
  (list 'if-let1 bindings (macroexp-progn body)))

(defmacro when-letn (bindings &rest body)
  "Usual `when'-like wrapper for `if-letn'."
  (declare (indent 1) (debug if-let-))
  (list 'if-letn bindings (macroexp-progn body)))

;; Not using simple aliases because `debug' declare form needs to be
;; adjusted as well.
;;
;; (defalias 'if-let 'if-let+)
;; (defalias 'when-let 'when-let+)

(defmacro if-let (&rest args)
  "Alias for `if-let+'."
  (declare (indent 2) (debug if-let-))
  `(if-let+ ,@args))

(defmacro when-let (&rest args)
  "Alias for `when-let+'."
  (declare (indent 1) (debug if-let-))
  `(when-let+ ,@args))

(defalias 'let- 'when-let-)

(provide 'let-macros)

;;; let-macros.el ends here
