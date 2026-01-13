;;; al-tempo.el --- My templates made with `tempo' engine  -*- lexical-binding: t -*-

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

;;; Code:

(require 'tempo)
(require 'al-general)

(defvar al/tempo-start ","
  "Starting string for all template tags.")

(defun al/tempo-tag (key)
  "Return tempo tag from KEY string."
  (concat al/tempo-start key))

(defvar al/tempo-regexp
  (rx-to-string `(and (group (regexp ,al/tempo-start)
                             (repeat 1 3 word))
                      point)
                'no-group)
  "Regexp for `tempo-match-finder'.")

(defvar-local al/tempo-configured nil
  "If `nil', then the current buffes has not been configured yet.

Otherwise, the current buffer has been configured and:

  if `t', then there are suitable templates;

  if `no-templates', then there are no suitable templates i.e., using
  `tempo-complete-tag' will do nothing.")

(defvar al/tempo-tags nil
  "List of variables defined for `tempo-use-tag-list'.")

(defvar al/tempo-alist
  '((al/tempo-elisp-case  . al/tempo-elisp-templates)
    (al/tempo-clisp-case  . al/tempo-clisp-templates)
    (al/tempo-scheme-case . al/tempo-scheme-templates))
  "Alist of (CASE . TEMPLATES) pairs for generating templates.

CASE is a function called without arguments.  It should check if the
current buffer is suitable for TEMPLATES.  If not, CASE should return
nil.  Otherwise, it should return (TAGLIST [ARGS]) list, where

  TAGLIST is a variable name for `tempo-define-template';

  ARGS is a list of arguments for TEMPLATES function.

TEMPLATES function should return a list of (TAG ELEMENTS NAME) entries
for `tempo-define-template'.  Alternatively, TEMPLATES can be a variable
with (TAG ELEMENTS NAME) entries.")


;;; Lisp and Scheme templates

(defun al/tempo-let-like-template (symbol-name key &optional prefix)
  "Return `let'-like template for SYMBOL-NAME."
  (let ((template-name (concat (or prefix "lisp-") symbol-name)))
    (list (al/tempo-tag key) template-name
          `("(" ,symbol-name " ((" p "))" n
            > r ")"))))

(defvar al/tempo-any-lisp-static-templates
  (append
   (mapcar (lambda (args)
             (apply #'al/tempo-let-like-template args))
           '(("let*"      "l")
             ("if-let"    "il")
             ("if-let1"   "il1")
             ("if-letn"   "iln")
             ("when-let"  "wl")
             ("when-let1" "wl1")
             ("when-letn" "wln")))
   `((,(al/tempo-tag "la") "lisp-lambda"
      ("(lambda (" p ")" n
       > r ")"))
     ))
  "Static templates for all Lisp family languages.")

(defvar al/tempo-lisp-static-templates
  nil
  "Static templates for Emacs Lisp and Common Lisp languages.")

(defun al/tempo-lisp-templates (&optional prefix)
  "Templates for Emacs Lisp and Common Lisp languages."
  (append
   al/tempo-any-lisp-static-templates
   al/tempo-lisp-static-templates
   `((,(al/tempo-tag "m") ,(concat prefix "lisp-macro")
      ("(defmacro " ,prefix p " ( &optional &rest)" n
       > "\"\"" n
       > "(declare (indent 0) (debug t))" n
       > ")"))
     (,(al/tempo-tag "f") ,(concat prefix "lisp-function")
      ("(defun " ,prefix p " ( &optional &rest)" n
       > "\"\"" n
       > ")"))
     (,(al/tempo-tag "v") ,(concat prefix "lisp-variable")
      ("(defvar " ,prefix p n
       > "\"\")"))
     )))

(defun al/tempo-clisp-templates (&optional prefix)
  "Templates for Common Lisp."
  (append
   (al/tempo-lisp-templates prefix)
   `((,(al/tempo-tag "fk") ,(concat prefix "clisp-key-function")
      ("(defun " ,prefix p " (&key)" n
       > "\"\"" n
       > ")"))
     (,(al/tempo-tag "ff") ,(concat prefix "clisp-function*")
      ("(defun " ,prefix p " (&key &optional &rest)" n
       > "\"\"" n
       > ")"))
     (,(al/tempo-tag "c") ,(concat prefix "stumpwm-command")
      ("(defcommand " ,prefix p " () ()" n
       > "\"\"" n
       > ")"))
     )))

(defun al/tempo-elisp-templates (&optional prefix)
  "Templates for Emacs Lisp."
  (append
   (al/tempo-lisp-templates prefix)
   `((,(al/tempo-tag "fk") ,(concat prefix "elisp-key-function")
      ("(cl-defun " ,prefix p " (&key)" n
       > "\"\"" n
       > ")"))
     (,(al/tempo-tag "ff") ,(concat prefix "elisp-function*")
      ("(cl-defun " ,prefix p " (&key &optional &rest)" n
       > "\"\"" n
       > ")"))
     (,(al/tempo-tag "c") ,(concat prefix "elisp-command*")
      ("(defun " ,prefix p " ()" n
       > "\"\"" n
       > "(interactive)" n
       > ")"))
     (,(al/tempo-tag "vl") ,(concat prefix "elisp-local-variable")
      ("(defvar-local " ,prefix p n
       > "\"\")"))
     )))

(defvar al/tempo-scheme-static-templates
  (append
   `((,(al/tempo-tag "m") "scheme-syntax-rule"
      ("(define-syntax-rule " p " ()" n
       > ")"))
     (,(al/tempo-tag "mm") "scheme-syntax-rules"
      ("(define-syntax " p n
       > "(syntax-rules ()" n
       > "((_ ()) )" n
       > "((_ ()) )" n
       > ")"))
     (,(al/tempo-tag "lam") "scheme-lambda*"
      ("(lambda* (#:key " p " #:optional #:rest)" n
       > r ")"))
     (,(al/tempo-tag "f") "scheme-function"
      ("(define (" p " )" n
       > ")"))
     (,(al/tempo-tag "fk") "scheme-key-function"
      ("(define* (" p "#:key)" n
       > ")"))
     (,(al/tempo-tag "ff") "scheme-function*"
      ("(define* (" p "#:key #:optional #:rest)" n
       > ")"))
     (,(al/tempo-tag "v") "scheme-variable"
      ("(define " p ")")))
   (list (al/tempo-let-like-template "let-values" "lv" "scheme-")))
  "Static templates for Scheme.")

(defun al/tempo-scheme-templates ()
  "Templates for Scheme."
  (append
   al/tempo-any-lisp-static-templates
   al/tempo-scheme-static-templates))

(defun al/tempo-clisp-case ()
  (and (derived-mode-p 'lisp-mode)
       '(al/tempo-clisp-tags "al/")))

(defun al/tempo-elisp-case ()
  (and (derived-mode-p 'emacs-lisp-mode)
       (let* ((file-name (and buffer-file-name
                              (file-name-base buffer-file-name)))
              (prefix (and file-name
                           (if (string-match "\\`al-" file-name)
                               "al/"
                             (concat file-name "-")))))
         (list (intern (concat "al/tempo-elisp-" prefix "tags"))
               prefix))))

(defun al/tempo-scheme-case ()
  (and (derived-mode-p 'scheme-mode)
       '(al/tempo-scheme-tags)))


;;; Additional `tempo' elements

(defun al/tempo-kill-ring-element (element)
  "Return the last killed string if ELEMENT is `k'."
  (and (eq element 'k)
       (car kill-ring)))

(al/pushnew tempo-user-element-functions #'al/tempo-kill-ring-element)


;;; Generating templates for the current buffer

(defun al/tempo-setup-buffer-maybe ()
  "Setup templates for the current buffer if needed.
Return nil, if there are no templates for the current buffer.
Return non-nil otherwise."
  (if al/tempo-configured
      (eq t al/tempo-configured)
    (al/tempo-setup-buffer)))

(defun al/tempo-setup-buffer ()
  "Setup templates for the current buffer.
Return nil, if there are no templates for the current buffer.
Return non-nil otherwise."
  (al/tempo-setup-buffer-1 al/tempo-alist))

(defun al/tempo-setup-buffer-1 (alist)
  "Recursive sub-procedure of `al/tempo-setup-buffer'."
  (pcase (car alist)
    ('()
     (setq al/tempo-configured 'no-templates)
     nil)
    (`(,case . ,templates)
     (pcase (funcall case)
       ('()
        (al/tempo-setup-buffer-1 (cdr alist)))
       (`(,var . ,args)
        (unless (boundp var)
          (set var nil)
          (al/pushnew al/tempo-tags var)
          (pcase-dolist (`(,tag ,name ,template)
                         (if (fboundp templates)
                             (apply templates args)
                           (symbol-value templates)))
            (tempo-define-template name template tag nil var)))
        (tempo-use-tag-list var)
        (setq tempo-match-finder al/tempo-regexp
              al/tempo-configured t))))))


;;; Interactive commands

;;;###autoload
(defun al/tempo-complete-maybe ()
  "Try to complete `tempo' template at point.
Return non-nil if completion succeeds, nil otherwise."
  (interactive)
  (and (al/tempo-setup-buffer-maybe)
       (let ((pos (point)))
         (tempo-complete-tag 'silent)
         (/= pos (point)))))

(defun al/tempo-refresh-templates ()
  "Remove all created templates."
  (interactive)
  (dolist (var al/tempo-tags)
    (makunbound var)))

(provide 'al-tempo)

;;; al-tempo.el ends here
