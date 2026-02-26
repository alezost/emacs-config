;;; al-complete.el --- Additional functionality for completion engine  -*- lexical-binding: t -*-

;; Copyright © 2025–2026 Alex Kost

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

(defvar al/completion-ignored-extensions completion-ignored-extensions
  "Original value of `completion-ignored-extensions'.
This variable is used to set `al/dired-ignored-extensions'.")

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

(provide 'al-complete)

;;; al-complete.el ends here
