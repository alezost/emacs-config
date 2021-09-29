;;; al-misc.el --- Miscellaneous additional functionality

;; Copyright © 2013–2021 Alex Kost

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

(require 'cl-lib)

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

(defun al/next-element (list &optional element)
  "Return next element from LIST.
If ELEMENT is nil or not in LIST, return the first element of LIST.
If ELEMENT is an element of LIST, return an element placed after it."
  (if element
      (or (cadr (memq element list))
          (car list))
    (car list)))

(cl-defun al/add-to-list-after (list-var after-element new-element &key test)
  "Add NEW-ELEMENT to LIST-VAR after the first occurrence of AFTER-ELEMENT.
If AFTER-ELEMENT does not exist, insert NEW-ELEMENT to the end of
LIST-VAR.
TEST key is `eq' by default."
  (let* (added
         (new (cl-mapcon
               (lambda (lst)
                 (let ((elt (car lst)))
                   (if (or added
                           (not (funcall (or test #'eq)
                                         elt after-element)))
                       (list elt)
                     (setq added t)
                     (list elt new-element))))
               (symbol-value list-var))))
    (set list-var
         (if added
             new
           (append (symbol-value list-var) (list new-element))))))

(defun al/warning (format-string &rest args)
  "Display a warning message."
  (apply #'message
         (concat "WARNING: " format-string)
         args))

(defun al/p (predicate val &optional message)
  "Return non-nil if PREDICATE returns non-nil on VAL.
Otherwise display warning MESSAGE on VAL and return nil."
  (or (funcall predicate val)
      (progn (and message (al/warning message val))
             nil)))

(defun al/every (predicate vals &optional message)
  "Return non-nil if PREDICATE returns non-nil on each element of VALS.
If VALS is not a list, call PREDICATE on this value."
  (if (and (listp vals)
           (not (functionp vals))) ; to avoid treating "(lambda …)" as list
      (cl-every (lambda (val)
                  (al/p predicate val message))
                vals)
    (al/p predicate vals message)))

(defun al/function? (object)
  "Non-nil if OBJECT is a function or a list of functions."
  (al/every #'functionp object
            "Unknown function '%S'."))

(defun al/bound? (object)
  "Non-nil if OBJECT is a bound symbol or a list of bound symbols."
  (al/every #'boundp object
            "Symbol '%S' is not bound."))

(defun al/file? (object)
  "Non-nil if OBJECT is an existing file or a list of directories."
  (al/every #'file-exists-p object
            "File '%s' does not exist."))

(defun al/directory? (object)
  "Non-nil if OBJECT is an existing directory or a list of directories."
  (al/every #'file-directory-p object
            "Directory '%s' does not exist."))

(defmacro al/with-check (&rest body)
  "Call rest of BODY if all checks are passed successfully.

BODY should start with checks (keyword arguments).  The following
keywords are available: `:fun'/`:var'/`:file'/`:dir'.  Each
keyword argument may be an object or a list of objects.  These
objects are checkced to be a proper function / a bound symbol /
an existing file / an existing directory.

Return nil if checks are not passed."
  (declare (indent 0) (debug (name body)))
  (let (fun var file dir)
    (while (keywordp (car body))
      (pcase (pop body)
        (`:fun  (setq fun  (pop body)))
        (`:var  (setq var  (pop body)))
	(`:file (setq file (pop body)))
	(`:dir  (setq dir  (pop body)))
	(_ (pop body))))
    `(when (and ,(or (null fun)  `(al/function?  ,fun))
                ,(or (null var)  `(al/bound?     ,var))
                ,(or (null file) `(al/file?      ,file))
                ,(or (null dir)  `(al/directory? ,dir)))
       ,@body)))

(defun al/funcall-or-dolist (val function)
  "Call FUNCTION on VAL if VAL is not a list.
If VAL is a list, call FUNCTION on each element of the list."
  (declare (indent 1))
  (if (listp val)
      (dolist (v val)
        (funcall function v))
    (funcall function val)))

(defun al/list-maybe (obj)
  "Return OBJ if it is a list, or a list with OBJ otherwise."
  (if (listp obj) obj (list obj)))

(defun al/assq-delete-all (keys alist &optional predicate)
  "Delete from ALIST all elements whose car is one of KEYS.
This is similar to `assq-delete-all', but KEYS can either be a
single key or a list of keys.  KEYS are checked using
PREDICATE (`memq' by default)."
  (let ((keys (al/list-maybe keys)))
    (cl-delete-if (lambda (assoc)
                    (and (consp assoc)
                         (funcall (or predicate #'memq)
                                  (car assoc)
                                  keys)))
                  alist)))

(defun al/assoc-delete-all (keys alist &optional predicate)
  "Delete from ALIST all elements whose car is one of KEYS.
This is similar to `assoc-delete-all', but KEYS can either be a
single key or a list of keys.  KEYS are checked using
PREDICATE (`member' by default)."
  (al/assq-delete-all keys alist #'member))

(defun al/add-to-load-path-maybe (&rest dirs)
  "Add existing directories from DIRS to `load-path'."
  (dolist (dir dirs)
    (al/with-check
      :dir dir
      (push dir load-path))))

(defun al/load (file)
  "Load FILE.
FILE may omit an extension.  See `load' for details."
  (or (load file 'noerror)
      (al/warning "Failed to load '%s'." file)))

(defun al/add-hook-maybe (hooks functions &optional append local)
  "Add all bound FUNCTIONS to all HOOKS.
Both HOOKS and FUNCTIONS may be single variables or lists of those."
  (declare (indent 1))
  (al/funcall-or-dolist functions
    (lambda (fun)
      (al/with-check
        :fun fun
        (al/funcall-or-dolist hooks
          (lambda (hook)
            (add-hook hook fun append local)))))))

(defun al/add-after-init-hook (functions)
  "Add functions to `after-init-hook'.
See `al/add-hook-maybe'."
  (al/add-hook-maybe 'after-init-hook functions))

(defmacro al/eval-after-init (&rest body)
  "Add to `after-init-hook' a `lambda' expression with BODY."
  (declare (indent 0))
  `(add-hook 'after-init-hook (lambda () ,@body)))

(defmacro al/define-package-exists (name &optional symbol)
  "Define `al/NAME-exists?' variable.
The value of the variable tells if SYMBOL is `fbound'.  If SYMBOL
is not specified, NAME is checked (both should be unquoted
symbols)."
  (let* ((name-str (symbol-name name))
         (var (intern (concat "al/" name-str "-exists?"))))
    `(defvar ,var (fboundp ',(or symbol name))
       ,(format "Non-nil, if `%s' package is available."
                name-str))))

(provide 'al-misc)

;;; al-misc.el ends here
