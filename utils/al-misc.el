;;; al-misc.el --- Miscellaneous additional functionality  -*- lexical-binding: t -*-

;; Copyright © 2013–2025 Alex Kost

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

(eval-when-compile (require 'cl-lib))
(require 'seq)
(require 'al-general)

(defun al/time-string-to-seconds (str)
  ;; This function originates from `org-emms-time-string-to-seconds'
  ;; (from `org-emms' package).
  "Convert timestring STR to a number of seconds.
STR can have one of the following formats:
- SS
- MM:SS
- HH:MM:SS
"
  (save-match-data
    (if (string-match "\\([0-9]+:\\)?\\([0-9]+\\):\\([0-9]+\\)" str)
	(let ((h (if (match-beginning 1)
                     (string-to-number (match-string 1 str))
                   0))
	      (m (string-to-number (match-string 2 str)))
	      (s (string-to-number (match-string 3 str))))
	  (+ (* h 3600) (* m 60) s))
      (string-to-number str))))

(defun al/intern (string-or-symbol)
  "Like `intern' except STRING-OR-SYMBOL can also be a symbol."
  (if (symbolp string-or-symbol)
      string-or-symbol
    (intern string-or-symbol)))

(defun al/next-element (list &optional element)
  "Return next element from LIST.
If ELEMENT is nil or not in LIST, return the first element of LIST.
If ELEMENT is an element of LIST, return an element placed after it."
  (if element
      (or (cadr (memq element list))
          (car list))
    (car list)))

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
    (unless (seq-find (lambda (elt)
                        (funcall test elt new-element))
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

(defmacro al/with-eval-to-kill-ring (&rest body)
  "Evaluate BODY and return its result.
If the result is string or symbol, put it into `kill-ring' and display
it in minibuffer."
  (declare (indent 0) (debug (name body)))
  (let ((res-var     (make-symbol "res"))
        (res-str-var (make-symbol "res-str")))
    `(let* ((,res-var (progn ,@body))
            (,res-str-var (cond
                           ((stringp ,res-var) ,res-var)
                           ((symbolp ,res-var) (symbol-name ,res-var)))))
       (when ,res-str-var
         (kill-new ,res-str-var)
         (message ,res-str-var))
       ,res-var)))

(provide 'al-misc)

;;; al-misc.el ends here
