;;; parens.el --- Additional commands for working with parentheses  -*- lexical-binding: t -*-

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

;;; Commentary:

;; This package provides some interactive commands to work with sexps
;; and parentheses.  I find `paredit-mode' and `smartparens-mode' very
;; inconvinient: they try to control too much and do unnecessary extra
;; stuff behind the scenes.  So instead of using one of them as a
;; stand-alone mode, I use some functionality they provide.  This
;; package is basically a wrapper for some of the `paredit' and
;; `smartparens' commands.  Finally, even if `paredit' and `smartparens'
;; are not available (for sure, a use-case interesting only for me),
;; this package still works providing restricted functionality thanks to
;; commands that come with Emacs itself (like `kill-word', `down-list',
;; etc.).

;; Provided commands:
;;
;; - `parens-skip-forward'
;; - `parens-skip-backward'
;; - `parens-forward-sexp'
;; - `parens-backward-sexp'
;; - `parens-forward-up-sexp'
;; - `parens-forward-down-sexp'
;; - `parens-backward-up-sexp'
;; - `parens-backward-down-sexp'
;; - `parens-forward'
;; - `parens-backward'
;; - `parens-forward-down'
;; - `parens-backward-up'
;; - `parens-transpose-sexps'
;; - `parens-kill-word-forward'
;; - `parens-kill-word-backward'
;; - `parens-kill-sexp-forward'
;; - `parens-kill-sexp-backward'

;;; Code:

(eval-when-compile (require 'cl-lib))

(eval-and-compile
  (defvar parens-packages-loaded-p
    (and (require 'paredit nil t)
         (require 'smartparens nil t))
    "Non-nil, if `paredit' and `smartparens' are loaded."))

(defun parens-assert-packages ()
  "Make sure `paredit' and `smartparens' are available.
If not, throw an error."
  (unless parens-packages-loaded-p
    (error "Cannot do this operation without `paredit' or `smartparens'")))

(defmacro parens-handle-scan-error (body &rest on-error-body)
  "Evaluate BODY expression.
If `scan-error' is signalled, evaluate ON-ERROR-BODY."
  (declare (indent 1) (debug t))
  `(condition-case nil
       ,body
     (scan-error ,@on-error-body)))


;;; Skipping parentheses

(defvar parens-string "()[]\""
  "String with parentheses skipped by `parens-skip'.")

(defun parens-skip (direction)
  "Skip parentheses at point and whitespaces after that.

If there are no parentheses at point, then skip whitespaces at first and
parentheses after that.

DIRECTION should be either `forward' or `backward' symbol.

Return non-nil, if something was skipped.  Return nil otherwise."
  (cl-flet ((skip (str)
              (abs (funcall (cl-ecase direction
                              (forward  #'skip-chars-forward)
                              (backward #'skip-chars-backward))
                            str))))
    (let* ((skipped1 (skip parens-string))
           (skipped2 (skip " \t\n"))
           (skipped3 (if (= 0 skipped1) (skip parens-string) 0))
           (skipped  (+ skipped1 skipped2 skipped3)))
      (> skipped 0))))

;;;###autoload
(defun parens-skip-forward ()
  "Skip parentheses at point forward and whitespaces after them.
See `parens-skip' for the returning value."
  (interactive)
  (parens-skip 'forward))

;;;###autoload
(defun parens-skip-backward ()
  "Skip parentheses at point backward and whitespaces before them.
See `parens-skip' for the returning value."
  (interactive)
  (parens-skip 'backward))


;;; Moving

(defvar parens-open-regexp (rx (or ?\[ ?\( ?\" ?`))
  "Regexp matching a single open parenthesis-like symbol.")

;;;###autoload
(defun parens-forward-sexp ()
  "Move forward across one sexp."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-forward)
    (forward-sexp)))

;;;###autoload
(defun parens-backward-sexp ()
  "Move backward across one sexp."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-backward)
    (backward-sexp)))

;;;###autoload
(defun parens-forward ()
  "Move forward across one sexp.

If impossible to move at the current level of parentheses, move forward
across the next sibling sexp i.e., move up, then move down, and move
forward again.

In any case, this function should be able to move until the end of
buffer."
  (interactive)
  (parens-handle-scan-error
      (forward-sexp)
    (parens-forward-up-sexp)
    (ignore-errors
      (parens-forward-down-sexp))
    (parens-forward)))

;;;###autoload
(defun parens-backward ()
  "Move backward across one sexp.

If impossible to move at the current level of parentheses, move backward
across the next sibling sexp i.e., move up, then move down, and move
backward again.

In any case, this function should be able to move until the beginning of
buffer."
  (interactive)
  (parens-handle-scan-error
      (backward-sexp)
    (parens-backward-up-sexp)
    (ignore-errors
      (parens-backward-down-sexp))
    (parens-backward)))

;;;###autoload
(defun parens-forward-up-sexp ()
  "Move forward up one level of parentheses."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-forward-up)
    (up-list)))

;;;###autoload
(defun parens-forward-down-sexp ()
  "Move forward down one level of parentheses."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-forward-down)
    (down-list)))

;;;###autoload
(defalias 'parens-backward-up-sexp #'backward-up-list)

;;;###autoload
(defun parens-backward-down-sexp ()
  "Move backward down one level of parentheses."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-backward-down)
    (down-list -1)))

(defun parens-inside-comment-or-string ()
  "Return non-nil, if point is inside comment or string."
  (or (ppss-comment-or-string-start (syntax-ppss))
      ;; ↑ this returns nil when the point is at the very beginning of a
      ;; commentary (e.g., on the starting ";" symbol in `lisp-mode'),
      ;; so we also check the next character.  XXX Is there a better way
      ;; to do this?
      (save-excursion
        (forward-char)
        (ppss-comment-or-string-start (syntax-ppss)))))

;;;###autoload
(defun parens-forward-down ()
  "Move forward down one level of parentheses.

This is similar to `parens-forward-down-sexp' except if it is impossible to
move down, then move forward up and down again.

Also this function tries to do something useful inside comments and
strings."
  (interactive)
  ;; `down-list' does not support strings and comments at all.
  ;; `paredit-forward-down' does not move down inside a string/comment
  ;; and doesn't support symbol quotes (`').  `sp-down-sexp' works
  ;; inside strings/comments but it is highly unreliable (it can
  ;; suddenly stuck or show "Search failed" message which can't be
  ;; handled because it is not an error).  So instead, we simply search
  ;; for parentheses inside a string/comment.
  (if (parens-inside-comment-or-string)
      (re-search-forward parens-open-regexp nil t)
    (parens-handle-scan-error
        (parens-forward-down-sexp)
      (parens-forward-up-sexp)
      (ignore-errors
        (parens-forward-down)))))

;;;###autoload
(defun parens-backward-up ()
  "Move backward up one level of parentheses.

This is similar to `parens-backward-up-sexp' except it also tries to do
something useful inside comments and strings."
  (interactive)
  (if (parens-inside-comment-or-string)
      (re-search-backward parens-open-regexp nil t)
    (call-interactively #'parens-backward-up-sexp)))


;;; Editing

;;;###autoload
(defun parens-transpose-sexps ()
  "Interchange sexps around point."
  (interactive)
  (if parens-packages-loaded-p
      (sp-transpose-sexp)
    (transpose-sexps 1)))

;;;###autoload
(defun parens-kill-word-forward ()
  "Kill word forward skipping parentheses if possible."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-forward-kill-word)
    (kill-word 1)))

;;;###autoload
(defun parens-kill-word-backward ()
  "Kill word backward skipping parentheses if possible."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-backward-kill-word)
    (backward-kill-word 1)))

;;;###autoload
(defun parens-kill-sexp-forward (&optional arg)
  "Kill sexp forward.
Similar to `kill-sexp', except if ARG is a raw prefix
\\[universal-argument], kill from point to the end of current
list/string, as `sp-kill-sexp' does."
  (interactive "P")
  (if (equal arg '(4))
      (progn
        (parens-assert-packages)
        (kill-sexp)
        (sp-kill-sexp arg))
    (kill-sexp (prefix-numeric-value arg))))

;;;###autoload
(defun parens-kill-sexp-backward (&optional arg)
  "Kill sexp backward.
Similar to `backward-kill-sexp', except if ARG is a raw prefix
\\[universal-argument], kill from point to the end of current
list/string, as `sp-backward-kill-sexp' does."
  (interactive "P")
  (if (equal arg '(4))
      (progn
        (parens-assert-packages)
        (backward-kill-sexp)
        (sp-backward-kill-sexp arg))
    (backward-kill-sexp (prefix-numeric-value arg))))

(provide 'parens)

;;; parens.el ends here
