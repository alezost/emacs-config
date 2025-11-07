;;; al-minibuffer.el --- Additional functionality for minibuffer  -*- lexical-binding: t -*-

;; Copyright © 2013–2025 Alex Kost

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

(provide 'al-minibuffer)

;;; al-minibuffer.el ends here
