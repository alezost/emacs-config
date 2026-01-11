;;; let-macros.el --- `if-let' and similar macros  -*- lexical-binding: t -*-

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
;; - `if-let',
;; - `if-let1',
;; - `if-letn',
;; - `when-let',
;; - `when-let1',
;; - `when-letn'.
;;
;; `if-let' / `when-let' are the same as `if-let*' / `when-let*'
;; provided by Emacs.  The only reason Emacs has "*" in these ↑ names
;; is that it already provides `if-let' / `when-let' which are
;; obsolete now.
;;
;; `*-let1' / `*-letn' are somewhat analogous to `prog1' / `progn'.
;; That's why they have these names.

;;; Code:

(defmacro if-let (&rest args)
  "The same as `if-let*'."
  (declare (indent 2) (debug if-let*))
  `(if-let* ,@args))

(defmacro when-let (&rest args)
  "The same as `when-let*'."
  (declare (indent 1) (debug if-let*))
  `(when-let* ,@args))

(defmacro if-let1 (bindings then &rest else)
  "Call `if-let' on the first binding and `let*' on the rest.

For example,

  (if-let1 ((a 1)
            (b 2)
            (c 3))
      (+ a b c)
    0)

is expanded to

  (if-let ((a 1))
      (let* ((b 2)
             (c 3))
        (+ a b c))
    0)
"
  (declare (indent 2) (debug if-let*))
  `(if-let (,(car bindings))
       (let* ,(cdr bindings)
         ,then)
     ,@else))

(defmacro if-letn (bindings then &rest else)
  "Call `if-let' on the last binding and `let*' on the rest.

For example,

  (if-letn ((a 1)
            (b 2)
            (c 3))
      (+ a b c)
    0)

is expanded to

  (let* ((a 1)
         (b 2))
    (if-let ((c 3))
        (+ a b c)
      0))
"
  (declare (indent 2) (debug if-let*))
  (let* ((reversed (nreverse bindings))
         (last (car reversed))
         (all-but-last (nreverse (cdr reversed))))
    `(let* ,all-but-last
       (if-let (,last)
           ,then
         ,@else))))

(defmacro when-let1 (bindings &rest body)
  "Wrapper for `if-let1'."
  (declare (indent 1) (debug if-let*))
  (list 'if-let1 bindings (macroexp-progn body)))

(defmacro when-letn (bindings &rest body)
  "Wrapper for `if-letn'."
  (declare (indent 1) (debug if-let*))
  (list 'if-letn bindings (macroexp-progn body)))

(provide 'let-macros)

;;; let-macros.el ends here
