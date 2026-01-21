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
;; - `parens-backward-up'
;; - `parens-backward-down'
;; - `parens-forward-up'
;; - `parens-forward-down'
;; - `parens-forward-down*'
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


;;; Skipping parentheses

(defvar parens-string "()[]\"\""
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
(defun parens-forward-up ()
  "Move forward up one level of parentheses."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-forward-up)
    (up-list)))

;;;###autoload
(defun parens-forward-down ()
  "Move forward down one level of parentheses."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-forward-down)
    (down-list)))

;;;###autoload
(defun parens-forward-down* ()
  "Move forward down into a list.
This is similar to `parens-forward-down' except if it is impossible to
move down, then move forward up and down again."
  (interactive)
  (condition-case nil
      (parens-forward-down)
    (error
     (if (looking-at ")")
         (progn
           (parens-forward-up)
           (parens-forward-down*))
       (message "Cannot move down")))))

;;;###autoload
(defun parens-backward-up ()
  "Move backward up one level of parentheses."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-backward-up)
    (backward-up-list)))

;;;###autoload
(defun parens-backward-down ()
  "Move backward down one level of parentheses."
  (interactive)
  (if parens-packages-loaded-p
      (paredit-backward-down)
    (down-list -1)))


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
