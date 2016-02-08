;;; al-scheme.el --- Additional functionality for scheme mode

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 1 Feb 2015

;;; Code:

(defvar utl-scheme-imenu-generic-expression
  `((nil
     ,(rx bol "(define"
          (zero-or-one "*")
          (zero-or-one "-public")
          (one-or-more space)
          (zero-or-one "(")
          (group (one-or-more (or word (syntax symbol)))))
     1)
    ("Methods"
     ,(rx bol "(define-"
          (or "generic" "method" "accessor")
          (one-or-more space)
          (zero-or-one "(")
          (group (one-or-more (or word (syntax symbol)))))
     1)
    ("Classes"
     ,(rx bol "(define-class"
          (one-or-more space)
          (zero-or-one "(")
          (group (one-or-more (or word (syntax symbol)))))
     1)
    ("Records"
     ,(rx bol "(define-record-type"
          (zero-or-one "*")
          (one-or-more space)
          (group (one-or-more (or word (syntax symbol)))))
     1)
    ("Conditions"
     ,(rx bol "(define-condition-type"
          (one-or-more space)
          (group (one-or-more (or word (syntax symbol)))))
     1)
    ("Modules"
     ,(rx bol "(define-module"
          (one-or-more space)
          (group "(" (one-or-more any) ")"))
     1)
    ("Macros"
     ,(rx bol "("
          (or (and "defmacro"
                   (zero-or-one "*")
                   (zero-or-one "-public"))
              "define-macro" "define-syntax" "define-syntax-rule")
          (one-or-more space)
          (zero-or-one "(")
          (group (one-or-more (or word (syntax symbol)))))
     1))
  "Improved substitution of `scheme-imenu-generic-expression'.")


(defvar calculate-lisp-indent-last-sexp)

;; The following code of `utl-scheme-indent-function' is taken from
;; <http://www.netris.org/~mhw/scheme-indent-function.el>.

(defun utl-scheme-indent-function (indent-point state)
  "Scheme mode function for the value of the variable `lisp-indent-function'.
This function is the same as `scheme-indent-function' except it
indents property lists properly."
  (let ((normal-indent (current-column)))
    (goto-char (1+ (elt state 1)))
    (parse-partial-sexp (point) calculate-lisp-indent-last-sexp 0 t)
    (if (and (elt state 2)
             (not (looking-at "\\sw\\|\\s_")))
        ;; car of form doesn't seem to be a symbol
        (progn
          (if (not (> (save-excursion (forward-line 1) (point))
                      calculate-lisp-indent-last-sexp))
              (progn (goto-char calculate-lisp-indent-last-sexp)
                     (beginning-of-line)
                     (parse-partial-sexp (point)
					 calculate-lisp-indent-last-sexp 0 t)))
          ;; Indent under the list or under the first sexp on the same
          ;; line as calculate-lisp-indent-last-sexp.  Note that first
          ;; thing on that line has to be complete sexp since we are
          ;; inside the innermost containing sexp.
          (backward-prefix-chars)
          (current-column))
      (let ((function (buffer-substring (point)
					(progn (forward-sexp 1) (point))))
	    method)
	(setq method (or (get (intern-soft function) 'scheme-indent-function)
			 (get (intern-soft function) 'scheme-indent-hook)))
	(cond ((or (eq method 'defun)
		   (and (null method)
			(> (length function) 3)
			(string-match "\\`def" function)))
	       (lisp-indent-defform state indent-point))
              ;; This next cond clause is the only change -mhw
	      ((and (null method)
                    (> (length function) 1)
                    ; The '#' in '#:' seems to get lost, not sure why
                    (string-match "\\`:" function))
               (let ((lisp-body-indent 1))
                 (lisp-indent-defform state indent-point)))
	      ((integerp method)
	       (lisp-indent-specform method state
				     indent-point normal-indent))
	      (method
		(funcall method state indent-point normal-indent)))))))

(provide 'al-scheme)

;;; al-scheme.el ends here
