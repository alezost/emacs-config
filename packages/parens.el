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

(defmacro parens-handle-scan-error (body &rest on-error-body)
  "Evaluate BODY expression.
If `scan-error' is signalled, evaluate ON-ERROR-BODY."
  (declare (indent 1) (debug t))
  `(condition-case nil
       ,body
     (scan-error ,@on-error-body)))


;;; Checking and loading `paredit' and `smartparens'

(defvar parens-paredit-loaded-p nil)
(defvar parens-smartparens-loaded-p nil)

(defun parens-paredit-loaded-p ()
  "Return non-nil if `paredit' is loaded."
  (or parens-paredit-loaded-p
      (setq parens-paredit-loaded-p
            (require 'paredit nil t))))

(defun parens-smartparens-loaded-p ()
  "Return non-nil if `smartparens' is loaded."
  (or parens-smartparens-loaded-p
      (setq parens-smartparens-loaded-p
            (require 'smartparens nil t))))

(defun parens-assert-paredit ()
  "Make sure `paredit' is available.
If not, throw an error."
  (unless (parens-paredit-loaded-p)
    (error "Cannot do this operation without `paredit'")))

(defun parens-assert-smartparens ()
  "Make sure `smartparens' is available.
If not, throw an error."
  (unless (parens-smartparens-loaded-p)
    (error "Cannot do this operation without `smartparens'")))


;;; Declarations of the used functions for byte-compiler

(declare-function paredit-forward               "paredit")
(declare-function paredit-forward-up            "paredit")
(declare-function paredit-forward-down          "paredit")
(declare-function paredit-backward              "paredit")
(declare-function paredit-backward-down         "paredit")
(declare-function paredit-backward-kill-word    "paredit")
(declare-function paredit-forward-kill-word     "paredit")
(declare-function sp-transpose-sexp             "smartparens")
(declare-function sp-kill-sexp                  "smartparens")
(declare-function sp-backward-kill-sexp         "smartparens")


;;; Skipping parentheses

(defvar parens-string "()[]{}<>\""
  "String with parentheses skipped by `parens-skip'.")

(defun parens-skip (direction)
  "Skip parentheses at point and whitespaces after that.

DIRECTION should be either `forward' or `backward' symbol.

Return non-nil, if something was skipped.  Return nil otherwise."
  (cl-flet ((skip (str)
              (abs (funcall (cl-ecase direction
                              (forward  #'skip-chars-forward)
                              (backward #'skip-chars-backward))
                            str))))
    (let* ((skipped1 (skip parens-string))
           (skipped2 (if (> skipped1 0)
                         (skip " \t\n")
                       0))
           (skipped  (+ skipped1 skipped2)))
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

(defvar parens-open-regexp (rx (or ?\[ ?\( ?{ ?< ?\" ?` ?‘ ?“))
  "Regexp matching a single open parenthesis-like symbol.")

(defvar parens-sexp-modes
  '(lisp-data-mode
    scheme-mode)
  "List of major modes where moving commands try to move by sexps.
In other modes, fallback to searching for `parens-open-regexp' while
moving up or down.")

;;;###autoload
(defun parens-forward-sexp ()
  "Move forward across one sexp."
  (interactive)
  (if (parens-paredit-loaded-p)
      (paredit-forward)
    (forward-sexp)))

;;;###autoload
(defun parens-backward-sexp ()
  "Move backward across one sexp."
  (interactive)
  (if (parens-paredit-loaded-p)
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
    ;; Error may happen if there are non-balanced parentheses (e.g., in
    ;; a diff buffer).
    (ignore-errors
      (parens-forward-up-sexp))
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
  (if (parens-paredit-loaded-p)
      (paredit-forward-up)
    (up-list)))

;;;###autoload
(defun parens-forward-down-sexp ()
  "Move forward down one level of parentheses."
  (interactive)
  (if (parens-paredit-loaded-p)
      (paredit-forward-down)
    (down-list)))

;;;###autoload
(defalias 'parens-backward-up-sexp #'backward-up-list)

;;;###autoload
(defun parens-backward-down-sexp ()
  "Move backward down one level of parentheses."
  (interactive)
  (if (parens-paredit-loaded-p)
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
        (ppss-comment-depth (syntax-ppss)))))

(defun parens-move-fallback-p ()
  "Return non-nil, if we should fallback to `parens-open-regexp' search."
  (or (not (derived-mode-p parens-sexp-modes))
      ;; `down-list' does not support strings and comments at all.
      ;; `paredit-forward-down' does not move down inside a
      ;; string/comment and doesn't support symbol quotes (`').
      ;; `sp-down-sexp' works inside strings/comments but it is highly
      ;; unreliable (it can suddenly stuck or show "Search failed"
      ;; message which can't be handled because it is not an error).  So
      ;; instead, we simply search for parentheses inside a
      ;; string/comment.
      (parens-inside-comment-or-string)))

;;;###autoload
(defun parens-forward-down ()
  "Move forward down one level of parentheses.

This is similar to `parens-forward-down-sexp' except if it is impossible to
move down, then move forward up and down again.

Also this function tries to do something useful inside comments and
strings."
  (interactive)
  (if (parens-move-fallback-p)
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
  (if (parens-move-fallback-p)
      (re-search-backward parens-open-regexp nil t)
    (call-interactively #'parens-backward-up-sexp)))


;;; Editing

;;;###autoload
(defun parens-transpose-sexps ()
  "Interchange sexps around point."
  (interactive)
  (if (parens-smartparens-loaded-p)
      (sp-transpose-sexp)
    (transpose-sexps 1)))

;;;###autoload
(defun parens-kill-word-forward ()
  "Kill word forward skipping parentheses if possible."
  (interactive)
  (if (parens-paredit-loaded-p)
      (paredit-forward-kill-word)
    (kill-word 1)))

;;;###autoload
(defun parens-kill-word-backward ()
  "Kill word backward skipping parentheses if possible."
  (interactive)
  (if (parens-paredit-loaded-p)
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
        (parens-assert-smartparens)
        ;; `sp-kill-sexp' kills the current sexp (if the point is inside
        ;; it) and all whitespaces.  So we add a fake sexp here, then
        ;; remove everything after it, and remove the fake.
        (insert " a ")
        (sp-kill-sexp arg)
        (delete-char -2))
    (kill-sexp (prefix-numeric-value arg) t)))

;;;###autoload
(defun parens-kill-sexp-backward (&optional arg)
  "Kill sexp backward.
Similar to `backward-kill-sexp', except if ARG is a raw prefix
\\[universal-argument], kill from point to the end of current
list/string, as `sp-backward-kill-sexp' does."
  (interactive "P")
  (if (equal arg '(4))
      (progn
        (parens-assert-smartparens)
        ;; We add a fake sexp here, then remove everything before it,
        ;; and remove the fake.
        (insert " a ")
        (backward-char 2)
        (sp-backward-kill-sexp arg)
        (delete-char 2))
    (backward-kill-sexp (prefix-numeric-value arg) t)))

(provide 'parens)

;;; parens.el ends here
