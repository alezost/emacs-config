;;; al-list.el --- List utilities  -*- lexical-binding: t -*-

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

(defun al/every-nth-element-1 (n list)
  "Sub-procedure of `al/every-nth-element'."
  (and list
       (cons (car list)
             (al/every-nth-element-1 n (nthcdr n list)))))

(defun al/every-nth-element (n list &optional start)
  "Return a list containing every Nth element from LIST.

START is the starting element (0 by default).

N must be a positive integer.  START must be a non-negative integer."
  (al/every-nth-element-1 n (if start (nthcdr start list) list)))

(provide 'al-list)

;;; al-list.el ends here
