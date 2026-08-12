;;; al-general.el --- Essential functionality for my config files  -*- lexical-binding: t -*-

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
  (require 'al-aux-macros)
  (require 'fp-utils))
(require 'seq)
(require 'al-places)


;;; `rx' definitions

;; `lisp-mode.el' defines `lisp-mode-symbol' rx name.  However, it is
;; not documented anywhere, so I define `al/lisp-symbol' instead.
(rx-define al/lisp-symbol
  (+ (or (syntax word) (syntax symbol))))

(rx-define al/lisp-symbol-group
  (group al/lisp-symbol))

(rx-define al/space
  (+ (or " " "\t")))

(rx-define al/space*
  (* (or " " "\t")))


;;; Simple wrappers for hooks

(defun al/show-trailing-whitespace ()
  (setq-local show-trailing-whitespace t))

(defun al/no-truncate-lines ()
  (setq-local truncate-lines nil))

(defun al/inhibit-field-motion ()
  (setq-local inhibit-field-text-motion t))

(defun al/bar-cursor-type ()
  (setq-local cursor-type 'bar))

(defun al/hbar-cursor-type ()
  (setq-local cursor-type 'hbar))

(defun al/no-syntactic-font-lock ()
  (setq-local font-lock-keywords-only t))

(defun al/set-comment-column ()
  (setq-local comment-column 32))

(defun al/set-default-paragraph ()
  (setq-local paragraph-start    (default-value 'paragraph-start)
              paragraph-separate (default-value 'paragraph-separate)))

(defun al/text-scale+1 ()
  "Increase the font size in the current buffer by 1."
  (text-scale-increase 1))


;;; Function utils

(defun al/funcall (function &rest arguments)
  "Call FUNCTION with ARGUMENTS.
This is the same as `funcall' except it returns nil instead of error if
FUNCTION does not exist."
  (and (al/function? function)
       (apply function arguments)))

(defun al/funcall-or-dolist (val function)
  "Call FUNCTION on VAL if VAL is not a list.
If VAL is a list, call FUNCTION on each element of the list."
  (declare (indent 1))
  (if (listp val)
      (dolist (v val)
        (funcall function v))
    (funcall function val)))


;;; Auxiliary messages

(defmacro al/define-message (name string)
  "Define `al/NAME-string' and `al/NAME-message' functions."
  (declare (indent 0) (debug t))
  (let* ((name-str (symbol-name name))
         (str-name (intern (concat "al/" name-str "-string")))
         (msg-name (intern (concat "al/" name-str "-message"))))
    `(progn
       (defun ,str-name (format-string &rest args)
         ,(concat "Return " name-str " string.")
         (apply #'format
                (concat ,string format-string)
                args))

       (defun ,msg-name (format-string &rest args)
         ,(concat "Display " name-str " message.")
         (message (apply #',str-name format-string args))))))

(al/define-message error "ERROR: ")
(al/define-message warning "WARNING: ")
(al/define-message important "XXX ")
(al/define-message title "⏺ ")


;;; Auxiliary predicates

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
      (seq-every-p (cut #'al/p predicate <> message)
                   vals)
    (al/p predicate vals message)))

(defun al/function? (object)
  "Non-nil if OBJECT is a function or a list of functions."
  (al/every? #'functionp object
             "Unknown function `%S'."))

(defun al/bound? (object)
  "Non-nil if OBJECT is a bound symbol or a list of bound symbols."
  (al/every? #'boundp object
             "Symbol `%S' is not bound."))

(defun al/file? (object)
  "Non-nil if OBJECT is an existing file or a list of directories."
  (al/every? #'file-exists-p object
             "File `%s' does not exist."))

(defun al/directory? (object)
  "Non-nil if OBJECT is an existing directory or a list of directories."
  (al/every? #'file-directory-p object
             "Directory `%s' does not exist."))

(defmacro al/with-check (&rest body)
  "Call rest of BODY if all checks are passed successfully.

BODY should start with checks (keyword arguments).  The following
keywords are available: `:fun'/`:var'/`:file'/`:dir'.  Each
keyword argument may be an object or a list of objects.  These
objects are checkced to be a proper function / a bound symbol /
an existing file / an existing directory.

Return nil if checks are not passed."
  (declare (indent 0) (debug (name body)))
  (al/with-keywords body
      (fun var file dir)
    `(when (and ,(or (null fun)  `(al/function?  ,fun))
                ,(or (null var)  `(al/bound?     ,var))
                ,(or (null file) `(al/file?      ,file))
                ,(or (null dir)  `(al/directory? ,dir)))
       ,@%body)))


;;; (Auto)loading utils

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
  (al/with-demoted-errors
      (concat "Failed to load `" file "': %S")
    (load file)))

(defun al/load-init (&rest files)
  "Load FILES from `al/emacs-init-dir'."
  (dolist (file files)
    (al/load (al/emacs-init-dir-file file))))

(defun al/load-settings (&rest files)
  "Load FILES from `al/emacs-settings-dir'."
  (dolist (file files)
    (al/load (al/emacs-settings-dir-file file))))

(defvar al/load-paths nil
  "List of `load-path' lists added by `al/load-autoloads'.")

(declare-function al/generate-autoloads "al-autoload")

(defun al/load-autoloads (name directory autoloads-file &rest args)
  "Load AUTOLOADS-FILE, generate it for DIRECTORY if needed.

NAME is a string used for messages.

Additional ARGS are sent to `al/generate-autoloads'.

Push added `load-path' to `al/load-paths'."
  (when (file-exists-p directory)
    (unless (file-exists-p autoloads-file)
      (al/with-demoted-errors
          (concat "Generating " name " autoloads failed: %S")
        (require 'al-autoload)
        (apply #'al/generate-autoloads directory
               :output-file autoloads-file
               args)))
    (al/with-demoted-errors
        (concat "Loading " name " autoloads failed: %S")
      (let ((count (length load-path)))
        (al/load autoloads-file)
        (push (seq-subseq load-path 0 (- count))
              al/load-paths)))))


;;; Miscellaneous utils

(defun al/intern (string-or-symbol)
  "Like `intern' except STRING-OR-SYMBOL can also be a symbol."
  (if (symbolp string-or-symbol)
      string-or-symbol
    (intern string-or-symbol)))

(provide 'al-general)

;;; al-general.el ends here
