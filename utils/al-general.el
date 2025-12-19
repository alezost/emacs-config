;;; al-general.el --- Additional functionality essential for my config  -*- lexical-binding: t -*-

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

(require 'seq)

(defmacro al/pushnew (place newelt &optional testfn)
  "Push NEWELT to PLACE if not already present.
This is similar to `cl-pushnew' but uses `seq' library instead of `cl-lib'."
  `(let ((elt ,newelt))
     (unless (seq-contains-p ,place elt ,testfn)
       (push elt ,place))))

(defun al/negate (fun)
  "Return a function that negates the result of FUN."
  (lambda (&rest args)
    (not (apply fun args))))

(defun al/list-maybe (obj)
  "Return OBJ if it is a list, or a list with OBJ otherwise."
  (if (listp obj) obj (list obj)))

(defun al/warning-message (format-string &rest args)
  "Display a warning message."
  (apply #'message
         (concat "WARNING: " format-string)
         args))

(defun al/important-message (format-string &rest args)
  "Display an important message."
  (apply #'message
         (concat "XXX " format-string)
         args))

(defun al/title-message (format-string &rest args)
  "Display title message."
  (apply #'message
         (concat "⏺ " format-string)
         args))

(defun al/p (predicate val &optional message)
  "Return non-nil if PREDICATE returns non-nil on VAL.
Otherwise display warning MESSAGE on VAL and return nil."
  (or (funcall predicate val)
      (progn (and message (al/warning-message message val))
             nil)))

(defun al/every? (predicate vals &optional message)
  "Return non-nil if PREDICATE returns non-nil on each element of VALS.
If VALS is not a list, call PREDICATE on this value."
  (if (and (listp vals)
           (not (functionp vals))) ; to avoid treating "(lambda …)" as list
      (seq-every-p (lambda (val)
                     (al/p predicate val message))
                   vals)
    (al/p predicate vals message)))

(defun al/function? (object)
  "Non-nil if OBJECT is a function or a list of functions."
  (al/every? #'functionp object
             "Unknown function '%S'."))

(defun al/bound? (object)
  "Non-nil if OBJECT is a bound symbol or a list of bound symbols."
  (al/every? #'boundp object
             "Symbol '%S' is not bound."))

(defun al/file? (object)
  "Non-nil if OBJECT is an existing file or a list of directories."
  (al/every? #'file-exists-p object
             "File '%s' does not exist."))

(defun al/directory? (object)
  "Non-nil if OBJECT is an existing directory or a list of directories."
  (al/every? #'file-directory-p object
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

(defun al/add-to-load-path-maybe (&rest dirs)
  "Add existing directories from DIRS to `load-path'."
  (dolist (dir dirs)
    (al/with-check
      :dir dir
      (push dir load-path))))

(defun al/load (file)
  "Load FILE.
Return t if FILE is loaded successfully, nil otherwise.
FILE may omit an extension.  See `load' for details."
  (when (stringp file)
    (or (load file 'noerror)
        (progn (al/warning-message "Failed to load '%s'." file)
               nil))))

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
  "Add to `after-init-hook' a `lambda' expression with BODY.
If `:append' keyword argument is specified, then the expression will be
added to the end/start of `after-init-hook' if `:append' value is t/nil
respectively."
  (declare (indent 0))
  (let ((depth nil))
    (while (keywordp (car body))
      (let ((keyword (pop body))
            (value   (pop body)))
        (cond
         ((eq keyword :append)
          ;; See documentation of `add-hook'.
          (setq depth (if value 100 -100)))
         (t
          (al/warning-message "Unknown keyword for `al/eval-after-init': %s"
                              keyword)))))
    `(add-hook 'after-init-hook (lambda () ,@body) ,depth)))

(provide 'al-general)

;;; al-general.el ends here
