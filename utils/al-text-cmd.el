;;; al-text-cmd.el --- Various interactive commands for working with text

;; Copyright © 2013-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:


;;; Searching and replacing

;;;###autoload
(defun al/re-search-forward (regexp)
  "The function is similar to `re-search-forward' except it continues
the search from the beginning of the buffer if it did not succeed."
  (interactive "sRegExp search: ")
  (let ((pos (point)))
    (or (re-search-forward regexp nil t)
	(progn
	  (goto-char (point-min))
	  (re-search-forward regexp pos t))
	(progn
	  (goto-char pos)
	  nil))))


;;; Editing

(defvar al/delimiter
  "\f\n"
  "String for separating text in elisp code.")

;;;###autoload
(defun al/insert-delimiter ()
  "Insert `al/delimiter' at point."
  (interactive)
  (insert al/delimiter))

;;;###autoload
(defun al/insert-date (&optional arg)
  "Insert date at point.
If ARG is nil, use current date.
If ARG is non-nil, prompt for a date."
  (interactive "P")
  (insert (if arg
              (org-read-date)
            (format-time-string "%Y-%m-%d"))))

;;;###autoload
(defun al/insert-clipboard ()
  "Insert the clipboard contents.
It doesn't destroy what you paste with \\[yank]."
  (interactive)
  (let ((clp (if (version< emacs-version "25")
                 (x-selection-value-internal 'CLIPBOARD)
               (gui--selection-value-internal 'CLIPBOARD))))
    (if clp
        (insert clp)
      (message "Clipboard is empty."))))

(defun al/yank-or-pop (n)
  "Replace just-yanked text with the N-th kill.
If last command is not `yank', call `yank' N times."
  (if (eq last-command 'yank)
      (yank-pop n)
    (dotimes (i (abs n)) (yank))))

;;;###autoload
(defun al/yank-or-prev (arg)
  "Replace just-yanked text with the previous kill.
See `al/yank-or-pop' for details."
  (interactive "p")
  (al/yank-or-pop arg))

;;;###autoload
(defun al/yank-or-next (arg)
  "Replace just-yanked text with the next kill.
See `al/yank-or-pop' for details."
  (interactive "p")
  (al/yank-or-pop (- arg)))

;;;###autoload
(defun al/flush-blank-lines (start end)
  "Delete all empty lines in selected region."
  (interactive "r")
  (flush-lines "^\\s-*$" start end nil))

;;;###autoload
(defun al/delete-blank-lines ()
  "Delete blank lines.
If region is active, call `al/flush-blank-lines',
otherwise call `delete-blank-lines'."
  (interactive)
  (if (region-active-p)
      (al/flush-blank-lines (region-beginning) (region-end))
    (delete-blank-lines)))

;;;###autoload
(defun al/kill-line (arg)
  "Similar to `kill-line' but kill including its terminating newline."
  (interactive "p")
  (kill-region (point)
               (progn (forward-visible-line arg) (point))))

;;;###autoload
(defun al/backward-kill-line (arg)
  "Kill line to its beginning.
With prefix argument ARG, kill that many lines backward including current."
  (interactive "p")
  (kill-region (point)
               (progn (forward-visible-line (- 1 arg)) (point))))

;;;###autoload
(defun al/save-line (arg)
  "Similar to `kill-line' but save in a kill ring without killing."
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion
                    (and arg (forward-visible-line (- arg 1)))
                    (end-of-visible-line)
                    (point))))

;;;###autoload
(defun al/backward-save-line (arg)
  "Similar to `al/backward-kill-line' but save in a kill ring without killing."
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion
                    (forward-visible-line (- 1 arg))
                    (point))))

;;;###autoload
(defun al/save-whole-line (arg)
  "Save current line as if killed, but don't kill it.
With ARG, save that many lines."
  (interactive "p")
  (save-excursion
    (and (< arg 0)
         (forward-visible-line 1))
    (kill-ring-save (point-at-bol)
                    (progn
                      (forward-visible-line arg)
                      (point)))))

;; Some ideas came from
;; <http://stackoverflow.com/questions/88399/how-do-i-duplicate-a-whole-line-in-emacs/>.
;;;###autoload
(defun al/duplicate-dwim (&optional n)
  "Duplicate current line, or region if it is active.
Leave the point on the last copy.
With argument N, make N copies.
With negative N, comment everything except the last copy."
  (interactive "*p")
  (or n (setq n 1))
  (let ((regionp (region-active-p)))
    (cl-multiple-value-bind (beg end col)
        (if regionp
            (list (region-beginning)
                  (region-end)
                  nil)
          (list (line-beginning-position)
                (line-beginning-position 2)
                (current-column)))
      ;; Save the point for undo.
      (setq buffer-undo-list
            (cons (point) buffer-undo-list))
      (let ((text (buffer-substring-no-properties beg end))
            (buffer-undo-list t))       ; disable undo
        (goto-char beg)
        (dotimes (i (abs n))
          (let ((beg (point)))
            (insert text)
            (when (< n 0)
              (comment-region beg (point))))))
      ;; Save bounds of the inserted region for undo.
      (setq buffer-undo-list
            (cons (cons beg (point)) buffer-undo-list))
      (or regionp (move-to-column col)))))

;;;###autoload
(defun al/save-word (arg)
  "Save characters forward until encountering the end of a word.
Save word as if killed, but don't kill it.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion (forward-word arg) (point))))

;;;###autoload
(defun al/backward-save-word (arg)
  "Save characters backward until encountering the end of a word.
Save word as if killed, but don't kill it.
With argument ARG, do this that many times."
  (interactive "p")
  (al/save-word (- (or arg 1))))

;;;###autoload
(defun al/save-sexp (arg)
  "Save characters forward until encountering the end of a sexp.
Save sexp as if killed, but don't kill it.
With argument ARG, do this that many times."
  (interactive "p")
  (kill-ring-save (point)
                  (save-excursion (forward-sexp arg) (point))))

;;;###autoload
(defun al/backward-save-sexp (arg)
  "Save characters backward until encountering the end of a sexp.
Save sexp as if killed, but don't kill it.
With argument ARG, do this that many times."
  (interactive "p")
  (al/save-sexp (- (or arg 1))))

;;;###autoload
(defun al/decode-region (beg end)
  "Replace selected text hexified by a browser with decoded one."
  (interactive "r")
  (let ((str (org-link-unescape
              (buffer-substring-no-properties beg end))))
    (delete-region beg end)
    (goto-char beg)
    (insert str)
    (message "String '%s' was decoded." str)))

(defun al/get-string (&optional msg)
  "Return a string from selected region or prompt for it.
Use message MSG in a prompt."
  (if (use-region-p)
      (buffer-substring-no-properties (region-beginning) (region-end))
    (read-string (or msg "Enter a string: "))))

;;;###autoload
(defun al/downcase-dwim (arg)
  "Use `downcase-region', if region is active, and `downcase-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (downcase-region (region-beginning) (region-end))
    (downcase-word arg)))

;;;###autoload
(defun al/upcase-dwim (arg)
  "Use `upcase-region', if region is active, and `upcase-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (upcase-region (region-beginning) (region-end))
    (upcase-word arg)))

;;;###autoload
(defun al/capitalize-dwim (arg)
  "Use `capitalize-region', if region is active, and `capitalize-word' otherwise."
  (interactive "p")
  (if (use-region-p)
      (capitalize-region (region-beginning) (region-end))
    (capitalize-word arg)))

;;;###autoload
(defun al/delete-horizontal-space (&optional direction)
  "Delete all spaces and tabs around point.
If DIRECTION is positive, delete them after point,
if it's negative - delete before point."
  (interactive "*P")
  (setq direction
        (cond
         ((listp direction) 0)
         ((or (and (equal '- direction))
              (and (numberp direction) (< direction 0)))
          -1)
         (t 1)))
  (let* ((cur (point))
         (beg (if (> direction 0)
                  cur
                (skip-chars-backward " \t")
                (constrain-to-field nil cur)))
         (end (if (< direction 0)
                  cur
                (skip-chars-forward " \t")
                (constrain-to-field nil cur t))))
    (delete-region beg end)))

;;;###autoload
(defun al/comment-dwirm (arg)
  "Call the comment command you want (Do What I Really Mean).
Similar to `comment-dwim' except if the region is not active,
call `comment-line'."
  (interactive "p")
  (if (use-region-p)
      (comment-dwim nil)
    (al/comment-line arg)))

;;;###autoload
(defun al/comment-line (arg)
  "Comment or uncomment current line.
If a prefix ARG is non-nil, use that many lines."
  (interactive "p")
  (or (> arg 0)
      (error "I don't want to comment previous lines"))
  (comment-or-uncomment-region (point-at-bol)
                               (point-at-eol arg)))

;;;###autoload
(defun al/dabbrev-expand-word (arg)
  "Expand current word.
Like `dabbrev-expand' but use word symbols only."
  (interactive "*P")
  (let ((dabbrev-abbrev-char-regexp "\\sw"))
    (dabbrev-expand arg)))


;;; Changing the case of previous word(s)

;; Idea from <http://www.emacswiki.org/emacs/sequential-command.el>.

;; Example of key bindings:
;;   (global-set-key (kbd "s-d") 'al/downcase-word-backward)
;;   (global-set-key (kbd "s-c") 'al/capitalize-word-backward)
;;   (global-set-key (kbd "s-u") 'al/upcase-word-backward)

;; When a key binding is pressed, the previous word is changed, if it
;; (or another key bound to those function) is pressed again, the word
;; before the previous is changed and so on.

(defvar al/word-position nil
  "Last saved position.
Used for `al/downcase-word-backward',
`al/capitalize-word-backward' and `al/upcase-word-backward'.")

(defvar al/word-seq-functions nil
  "List of commands for sequential modifying the case of a word.")

(defmacro al/change-word-backward (name fun)
  "Make a function for sequential changing previous word(s).
Resulting function `al/NAME-word-backward' will be added to
`al/word-seq-functions'.
Function FUN is called in body of the resulting function for updating
the word.  It should accept a number of modified words as argument."
  (let ((fun-name (intern (concat "al/" name "-word-backward"))))
    (add-to-list 'al/word-seq-functions fun-name)
    `(defun ,fun-name (arg)
       ,(concat (capitalize name)
                " previous word (or ARG words), do not move the point.\n"
                "Multiple calls will change previous words sequentially.")
       (interactive "p")
       (save-excursion
         (when (memq last-command al/word-seq-functions)
           (goto-char al/word-position))
         (backward-word arg)
         (setq al/word-position (point))
         (,fun arg)))))

(al/change-word-backward "downcase" downcase-word)
(al/change-word-backward "capitalize" capitalize-word)
(al/change-word-backward "upcase" upcase-word)

;;;###autoload (autoload 'al/downcase-word-backward "al-text" nil t)
;;;###autoload (autoload 'al/capitalize-word-backward "al-text" nil t)
;;;###autoload (autoload 'al/upcase-word-backward "al-text" nil t)


;;; Moving

;;;###autoload
(defun al/beginning-of-line ()
  "Move point to beginning of current line.
If the point is in the beginning of line already,
move to beginning of previous one."
  (interactive)
  (beginning-of-line (if (= (point) (point-at-bol)) 0 1)))

;;;###autoload
(defun al/end-of-line ()
  "Move point to end of current line.
If the point is in the end of line already,
move to end of next one."
  (interactive)
  (end-of-line (if (= (point) (point-at-eol)) 2 1)))

;;;###autoload
(defun al/recenter-top ()
  "Move current line to the top (+1) of the window."
  (interactive)
  (recenter-top-bottom 1))

;;;###autoload
(defun al/recenter-end-of-buffer-top ()
  "Move the last line (-1) of the buffer to the top of the window."
  (interactive)
  (goto-char (point-max))
  (recenter-top-bottom 0)
  (forward-line -2))


;;; Misc

(defvar al/check-parens-modes
  '(emacs-lisp-mode lisp-mode scheme-mode)
  "List of modes where `al/check-parens' is called.")

;;;###autoload
(defun al/check-parens ()
  "Run `check-parens' if current mode is one of `al/check-parens-modes'."
  (when (memq major-mode al/check-parens-modes)
    (check-parens)))

(provide 'al-text-cmd)

;;; al-text-cmd.el ends here
