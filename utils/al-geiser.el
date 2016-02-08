;;; al-geiser.el --- Additional functionality for geiser

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 13 Apr 2014

;;; Code:

(require 'geiser-mode)

;;;###autoload
(defun al/geiser-eval-dwim (arg)
  "Eval (with geiser) last sexp or region if it is active.
ARG is passed to `geiser-eval-last-sexp'."
  (interactive "P")
  (if (use-region-p)
      (geiser-eval-region (region-beginning) (region-end))
    (geiser-eval-last-sexp arg)))

;;;###autoload
(defun al/geiser-repl-enter-dwim ()
  "Send input or goto the error at point.
Substitution for `geiser-repl--maybe-send'."
  (interactive)
  (cond ((< (point) (geiser-repl--last-prompt-start))
         (if (geiser-repl--is-history-input)
             (geiser-repl--grab-input)
           (ignore-errors (compile-goto-error))))
        (t
         (geiser-repl--send-input))))

;;;###autoload
(defun al/geiser-repl-kill-whole-line (arg)
  "Similar to `kill-whole-line', but respect geiser repl prompt."
  (interactive "p")
  (kill-region (comint-line-beginning-position)
               (progn (forward-line arg) (point))))

;;;###autoload
(defun al/geiser-doc-doc-symbol-at-point ()
  "Open documentation for symbol at point.
This function refers to `geiser-doc-symbol-at-point' as
`geiser-doc-edit-symbol-at-point' refers to
`geiser-edit-symbol-at-point'."
  (interactive)
  (let* ((impl (geiser-doc--implementation))
         (module (geiser-doc--module)))
    (unless (and impl module)
      (error "I don't know what module this buffer refers to."))
    (with--geiser-implementation impl
      (geiser-doc-symbol-at-point))))

(defun al/geiser-repl-buffer-name (impl)
  "Return buffer name of Geiser REPL for IMPL."
  (format "*%s*" (geiser-repl--repl-name impl)))

(provide 'al-geiser)

;;; al-geiser.el ends here
