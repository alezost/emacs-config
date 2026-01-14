;;; al-minibuffer.el --- Additional functionality for minibuffer  -*- lexical-binding: t -*-

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

(require 'let-macros)

(defun al/completion-all-completions (fun string table pred point
                                      &optional metadata)
  "Call FUN on the rest args except use 0 instead of POINT.
This function is intended to be used as an `around' advice for
`completion-all-completions' function.

Evaluate this:

  (completion-all-completions \"ab\" \\='(\"abc\" \"xabc\" \"xxx\") nil 2)

Without this advice, it will return only (\"abc\").
With this advice, it will return (\"abc\" \"xabc\").

I prefer the latter because I want to see all completions matching my
input, not just some of them."
  (let ((point (if (and metadata
                        ;; File completions are broken if POINT is 0, so
                        ;; fallback to default.
                        (eq 'file
                            (completion-metadata-get metadata 'category)))
                   point
                 0)))
    (funcall fun string table pred point metadata)))

(defun al/completion-styles (&rest _)
  "Return `completion-styles'.
This function is intended to be a replacement for `completion--styles',
which adds default styles from `completion-category-defaults' to
the output."
  completion-styles)


;;; Additional minibuffer keymaps

(defvar-keymap al/minibuffer-buffer-map
  :doc "Additional keys to read buffer name from minibuffer.")

(defvar-keymap al/minibuffer-file-map
  :doc "Additional keys to read file name from minibuffer.")

(defvar-keymap al/minibuffer-symbol-map
  :doc "Additional keys to read symbol from minibuffer.")

(defmacro al/minibuffer-with-keymap (keymap &rest body)
  "Use KEYMAP to the next minibuffer call and run BODY."
  (declare (indent 1) (debug t))
  `(minibuffer-with-setup-hook
       (lambda ()
         (use-local-map
          (make-composed-keymap ,keymap (current-local-map))))
     ,@body))

(defun al/read-buffer-add-keymap (fun &rest args)
  "Add `al/minibuffer-buffer-map' to the local map and call FUN with ARGS.
This function is intendend to be used as an `around' advice for
`read-buffer'."
  (al/minibuffer-with-keymap al/minibuffer-buffer-map
    (apply fun args)))

(defun al/read-file-add-keymap (fun &rest args)
  "Add `al/minibuffer-file-map' to the local map and call FUN with ARGS.
This function is intendend to be used as an `around' advice for
`read-file-name'."
  (al/minibuffer-with-keymap al/minibuffer-file-map
    (apply fun args)))

(defun al/read-symbol-add-keymap (fun &rest args)
  "Add `al/minibuffer-symbol-map' to the local map and call FUN with ARGS.
This function is intendend to be used as an `around' advice for
procedures reading symbols from minibuffer."
  (al/minibuffer-with-keymap al/minibuffer-symbol-map
    (apply fun args)))


;;; Minibuffer fallback

(defvar al/minibuffer-fallback nil
  "Function that is called by `al/minibuffer-fallback-or-funcall'.
This variable should be set by minibuffer commands that want to exit
from minibuffer and continue execution.")

(defun al/minibuffer-fallback-or-funcall (fun &rest args)
  "Call `al/minibuffer-fallback' or apply FUN to ARGS.
This function is intendend to be used as an `around' advice for commands
that read from minibuffer in the interactive clause.  When a minibuffer
command sets `al/minibuffer-fallback' and exits from minibuffer, this
advice call `al/minibuffer-fallback' instead executing FUN body."
  (if al/minibuffer-fallback
      (let ((fallback al/minibuffer-fallback))
        (setq al/minibuffer-fallback nil)
        (funcall fallback))
    (apply fun args)))


;;; `al/split' completion style

(defun al/completion-make-split-pattern (pattern)
  ;; Originates from `completion-flex--make-flex-pattern'.
  "Split PCM-style PATTERN by words separated with spaces and hyphens.

This turns
    (prefix \"ab-cd ef\")
into
    (prefix \"ab\" any \"cd\" any \"ef\" any)"
  (mapcan (lambda (elem)
            (if (stringp elem)
                (mapcan (lambda (str)
                          (list str 'any))
                        (split-string elem "[ -]+"))
              (list elem)))
          pattern))

(defalias 'al/completion-split-try-completion
  ;; Default for `partial-completion' style.
  #'completion-pcm-try-completion)

(defun al/completion-split-all-completions (string table pred point)
  ;; Originates from `completion-substring--all-completions' and
  ;; `completion-flex-all-completion'.
  "Get completions of STRING in TABLE, given PRED and POINT."
  (when-letn
      ((beforepoint   (substring string 0 point))
       (afterpoint    (substring string point))
       (bounds        (completion-boundaries beforepoint table
                                             pred afterpoint))
       (prefix        (substring beforepoint 0 (car bounds)))
       (basic-pattern (completion-basic--pattern
                       beforepoint afterpoint bounds))
       (pattern       (if (not (stringp (car basic-pattern)))
                          basic-pattern
                        (cons 'prefix basic-pattern)))
       (pattern       (al/completion-make-split-pattern pattern))
       (all           (completion-pcm--all-completions
                       prefix pattern table pred)))
    (nconc (completion-pcm--hilit-commonality pattern all)
           (length prefix))))

(when (boundp 'completion-styles-alist)
  (push '(al/split
          al/completion-split-try-completion
          al/completion-split-all-completions
          "Completion of multiple substrings separated by spaces or hyphens.
When completing \"one two three\" the glob \"*one*two*three*\" is used,
so that \"a b\" can complete to \"ax yb\".")
        completion-styles-alist))

(provide 'al-minibuffer)

;;; al-minibuffer.el ends here
