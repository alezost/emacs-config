;;; count.el --- Counting utilities  -*- lexical-binding: t -*-

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

;; This file provides the following predicates to check a NUMBER, a
;; number of elements in a LIST/ARRAY, or a number of characters in a
;; STRING:
;;
;; - `=0',
;; - `/=0',
;; - `>0',
;; - `<0',
;; - `=1',
;; - `>1'.

;;; Code:

(eval-when-compile (require 'cl-generic))

(cl-defgeneric =0 (object)
  "Return non-nil if OBJECT has zero elements.")

(cl-defmethod =0 ((number number))
  "Return non-nil if NUMBER is zero."
  (zerop number))

(cl-defmethod =0 ((string string))
  "Return non-nil if STRING is empty."
  (string-empty-p string))

(cl-defmethod =0 ((list list))
  "Return non-nil if LIST is empty."
  (null list))

(cl-defmethod =0 ((array array))
  "Return non-nil if ARRAY length is zero."
  (length= array 0))

(cl-defgeneric /=0 (object)
  "Return non-nil if OBJECT has non-zero number of elements."
  (not (=0 object)))

;; `<0' exists only for numbers because it does not make sense to check
;; a sequence for negative number of elements.
(defun <0 (number)
  "Return non-nil if NUMBER is negative."
  (> 0 number))

(cl-defgeneric >0 (object)
  "Return non-nil if OBJECT has more than zero elements.")

(cl-defmethod >0 ((number number))
  "Return non-nil if NUMBER is positive."
  (< 0 number))

(cl-defmethod >0 ((sequence sequence))
  "Return non-nil if SEQUENCE length is greater than zero."
  (length> sequence 0))

(cl-defmethod >0 ((string string))
  "Return non-nil if STRING not empty."
  (not (string-empty-p string)))

(cl-defmethod >0 ((list list))
  "Return non-nil if LIST is not empty."
  list)

(cl-defgeneric =1 (object)
  "Return non-nil if OBJECT has one element.")

(cl-defmethod =1 ((number number))
  "Return non-nil if NUMBER is one."
  (= 1 number))

(cl-defmethod =1 ((sequence sequence))
  "Return non-nil if SEQUENCE length is equal to one."
  (length= sequence 1))

(cl-defgeneric >1 (object)
  "Return non-nil if OBJECT has more than one elements.")

(cl-defmethod >1 ((number number))
  "Return non-nil if NUMBER is greater than one."
  (< 1 number))

(cl-defmethod >1 ((list list))
  "Return non-nil if LIST length is greater than one."
  (cdr list))

(cl-defmethod >1 ((sequence sequence))
  "Return non-nil if SEQUENCE length is greater than one."
  (length> sequence 1))

(provide 'count)

;;; count.el ends here
