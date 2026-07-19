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

(provide 'fp-utils)

;;; fp-utils.el ends here
