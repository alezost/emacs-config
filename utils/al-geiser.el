;;; al-geiser.el --- Additional functionality for geiser

;; Copyright © 2014–2017 Alex Kost

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

(require 'geiser-mode)

(defun al/geiser-repl ()
  "Return the current Geiser REPL."
  (or (geiser-repl--repl/impl 'guile)
      (error "Geiser REPL not found")))

(defun al/geiser-eval (str &optional repl)
  "Evaluate STR with guile expression using Geiser REPL.
If REPL is nil, use the current Geiser REPL.
Return a list of strings with result values of evaluation."
  (with-current-buffer (or repl (al/geiser-repl))
    (let ((res (geiser-eval--send/wait `(:eval (:scm ,str)))))
      (if (geiser-eval--retort-error res)
          (error "Error in evaluating guile expression: %s"
                 (geiser-eval--retort-output res))
        (cdr (assq 'result res))))))

;;;###autoload
(defun al/geiser-add-to-load-path (directory)
  "Add DIRECTORY to the Guile REPL load paths."
  (interactive "DDirectory to add: ")
  (al/geiser-eval
   (concat "(set! %load-path "
           "(cons \"" directory "\" %load-path))"))
  (al/geiser-eval
   (concat "(set! %load-compiled-path "
           "(cons \"" directory "\" %load-compiled-path))")))

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


;;; Connecting to pre-defined sockets

(defvar al/geiser-sockets nil
  "List of Guile's socket files used by `al/geiser-socket-connect'.")

;;;###autoload
(defun al/geiser-socket-connect (socket)
  "Connect Geiser to Guile's SOCKET file.
Interactively, prompt for SOCKET using completions from
`al/geiser-sockets'."
  (interactive
   (list (expand-file-name
          (completing-read "Socket: " al/geiser-sockets))))
  (geiser-connect-local 'guile socket))

(provide 'al-geiser)

;;; al-geiser.el ends here
