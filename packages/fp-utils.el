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
;; - `and=<',
;; - `negate',
;; - `compose',
;; - `compose-left',
;; - `compose-right',
;; - `multi-filter'.

;;; Code:

(defun and=> (value &rest functions)
  "Return result of consecutive applying FUNCTIONS to VALUE.

More precisely, for a given list of FUNCTIONS, (FUN1 FUN2 ... FUNn),
return nil if:

  VALUE is nil or
  (FUN1 VALUE) is nil or
  (FUN2 (FUN1 VALUE)) is nil or
  ...
  (FUNn ... (FUN2 (FUN1 VALUE))) is nil.

Otherwise, return (FUNn ... (FUN2 (FUN1 VALUE)))."
  (and value
       (if functions
           (apply #'and=>
                  (funcall (car functions) value)
                  (cdr functions))
         value)))

(defmacro and<= (value &rest functions)
  "Return VALUE if (FUN VALUE) is not false for all FUNCTIONS.

More precisely, for a given list of FUNCTIONS, (FUN1 FUN2 ... FUNn),
return nil if:

  (FUN1 VALUE) is nil or
  (FUN2 VALUE) is nil or
  ...
  (FUNn VALUE) is nil.

Otherwise, return VALUE."
  (let ((var (make-symbol "val")))
    `(let ((,var ,value))
       (and ,@(mapcar (lambda (fun)
                        `(funcall ,fun ,var))
                      functions)
            ,var))))

(defun negate (fun)
  "Return a function that negates the result of FUN."
  (lambda (&rest args)
    (not (apply fun args))))

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

(defun multi-filter (element filters)
  "Pass ELEMENT through FILTERS.

FILTERS is a list of functions, (F1 F2 ... FN), applied from left to
right, passing result to the next function i.e.,

  (FN (... (F2 (F1 ELEMENT))))

If any filter returns nil, the rest filters are not applied.

Return result of the final filter application."
  (if (null filters)
      element
    (when-let* ((res (funcall (car filters) element)))
      (multi-filter res (cdr filters)))))

(provide 'fp-utils)

;;; fp-utils.el ends here
