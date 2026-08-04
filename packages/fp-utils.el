;;; fp-utils.el --- Basic functional programming utilities  -*- lexical-binding: t -*-

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

;; This file provides some functional programming utilities
;; that Emacs lacks:
;;
;; - `and=>',
;; - `and<=',
;; - `compose-funcall',
;; - `cut',
;; - `negate',
;; - `compose',
;; - `compose-left',
;; - `compose-right'.

;;; Code:

(defmacro compose-funcall (value &rest functions)
  "Return result of consecutive applying FUNCTIONS to VALUE.

More precisely, (compose-funcall VALUE #\\='F1 #\\='F2 ... #\\='Fn)
expands to (Fn ... (F2 (F1 VALUE))).

Unlike `and=>', `compose-funcall' does not check intermediate results
for nil."
  (declare (indent 1) (debug t))
  (if functions
      `(compose-funcall (funcall ,(car functions) ,value)
         ,@(cdr functions))
    value))

(defmacro and=> (value &rest functions)
  "Return result of consecutive applying FUNCTIONS to VALUE.

More precisely, for a given list of FUNCTIONS, (F1 F2 ... Fn),
return nil if:

  VALUE is nil or
  (F1 VALUE) is nil or
  (F2 (F1 VALUE)) is nil or
  ...
  (Fn ... (F2 (F1 VALUE))) is nil.

Otherwise, return (Fn ... (F2 (F1 VALUE))).

See also `compose-funcall' which does the same without checking
intermediate results for nil."
  (if (null functions)
      value
    (let ((var (make-symbol "val")))
      `(let ((,var ,value))
         (and ,var
              (and=> (funcall ,(car functions) ,var)
                     ,@(cdr functions)))))))

(defmacro and<= (value &rest functions)
  "Return VALUE if (FUN VALUE) is non-nil for all FUNCTIONS.

More precisely, for a given list of FUNCTIONS, (F1 F2 ... Fn),
return nil if:

  VALUE is nil or
  (F1 VALUE) is nil or
  (F2 VALUE) is nil or
  ...
  (Fn VALUE) is nil.

Otherwise, return VALUE."
  (let ((var (make-symbol "val")))
    `(let ((,var ,value))
       (and ,var
            ,@(mapcar (lambda (fun)
                        `(funcall ,fun ,var))
                      functions)
            ,var))))

(defmacro cut (&rest args)
  "Return a function to call (ARG1 REST-ARGS ...) with selected arguments.
Any argument can be `<>', the last argument can be `<...>'.
See Info node `(guile) SRFI-26' for details."
  (let* ((lambda-args '())
         (args (mapcar (lambda (arg)
                         (if (eq arg '<>)
                             (let ((sym (gensym)))
                               (push sym lambda-args)
                               sym)
                           arg))
                       args))
         (lambda-args (nreverse lambda-args))
         (call-expr `(funcall ,@args)))
    (when (eq (car (last args)) '<...>)
      (let ((last-sym (gensym)))
        (setq lambda-args (append lambda-args `(&rest ,last-sym))
              call-expr `(apply ,@(butlast args) ,last-sym))))
    `(lambda ,lambda-args ,call-expr)))

(defun negate (fun)
  "Return a function that negates the result of FUN."
  (lambda (&rest args)
    (not (apply fun args))))

(defun multi-filter (value &rest functions)
  "Return result of consecutive applying FUNCTIONS to VALUE.
This is the same as `and=>' except it is a function not a macro."
  (and value
       (if functions
           (apply #'multi-filter
                  (funcall (car functions) value)
                  (cdr functions))
         value)))

(defun compose (functions &optional direction)
  "Compose FUNCTIONS into a single function.

DIRECTION should be one of the following symbols: `left' (default) or
`right'.

If DIRECTION is `left', FUNCTIONS are composed from left to right i.e.,
the first function is applied to arguments, then the second function is
applied to the result, and so on.

If DIRECTION is `right', FUNCTIONS are composed from right to left i.e.,
the last function is applied to arguments, then the function before it
is applied to the result, and so on."
  (cond
   ((null functions)
    #'identity)
   ((null (cdr functions))
    (car functions))
   (t
    (let ((functions (if (eq direction 'right)
                         (reverse functions)
                       functions)))
      (lambda (&rest args)
        (let ((res (apply (car functions) args)))
          (dolist (fun (cdr functions))
            (setq res (funcall fun res)))
          res))))))

(defun compose-left (&rest functions)
  "Compose FUNCTIONS from left to right into a single function.
See `compose' for details."
  (compose functions 'left))

(defun compose-right (&rest functions)
  "Compose FUNCTIONS from right to left into a single function.
See `compose' for details."
  (compose functions 'right))

(provide 'fp-utils)

;;; fp-utils.el ends here
