;;; al-eshell.el --- Additional functionality for eshell  -*- lexical-binding: t -*-

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

(require 'em-dirs)
(require 'em-unix)
(require 'em-prompt)

(defun al/eshell-kill-whole-line (arg)
  "Similar to `kill-whole-line', but respect eshell prompt."
  (interactive "p")
  (if (< (point) eshell-last-output-end)
      (kill-whole-line arg)
    (kill-region eshell-last-output-end
                 (progn (forward-line arg) (point)))))

;;;###autoload
(defun al/eshell-cd (arg)
  "Start eshell and change directory there to the current one.
ARG has the same meaning as in `eshell'"
  (interactive "P")
  (let ((dir default-directory))
    (eshell arg)
    (eshell/cd dir)))

(declare-function Info-find-node "info" t)
(declare-function Info-menu "info" t)

(defun al/eshell/info (&rest args)
  "Run `info' command on NAME.
NAME is the car of ARGS.

This function is intended to be used as a substitution for
`eshell/info'.  It does the following:

  info       =>  go to top info window;
  info NAME  =>  if NAME is a file '*.info', visit it;
  info NAME  =>  otherwise go to top info node and then menu item NAME."
  (require 'info)
  (let* ((name (car args))
         (file (and (stringp name)
                    (string-match "\\.info" name)
                    (expand-file-name name))))
    (if (and file (file-exists-p file))
        (Info-find-node file "Top")
      (Info-directory)
      (Info-menu name))))


;;; Prompt

;; Idea from <http://www.emacswiki.org/emacs/EshellPrompt>.

;; TODO improve regexp
(defvar al/eshell-prompt-regexp "^[#$] "
  "Regexp for `eshell-prompt-regexp'.")

(defmacro al/with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun al/eshell-prompt ()
  "Function for `eshell-prompt-function'."
  (format "%s %s%s%s %s\n%s "
          (al/with-face (format-time-string "%H:%M" (current-time))
                        'font-lock-comment-face)
          (eshell/whoami)
          (al/with-face "@"
                        'escape-glyph)
          (system-name)
          (al/with-face (abbreviate-file-name (eshell/pwd))
                        'dired-directory)
          (al/with-face (if (= (user-uid) 0) "#" "$")
                        'comint-highlight-prompt)))


;;; Input (command) line

(defun al/eshell-input-at-point ()
  "Return eshell input from the current input (command) line.
Return nil, if the current line is not the input line."
  (let ((bol (pos-bol))
        (eol (pos-eol)))
    (and (eq 'prompt (get-text-property bol 'field))
         (null (get-text-property eol 'field))
         (buffer-substring-no-properties
          (save-excursion (goto-char eol) (line-beginning-position))
          eol))))

;;;###autoload
(defun al/eshell-send-input-maybe ()
  "Call `eshell-send-input' if the point is on the command line."
  (interactive)
  (when (< (point) eshell-last-output-end)
    (if-let* ((input (al/eshell-input-at-point)))
        (progn
          (goto-char eshell-last-output-end)
          (delete-region eshell-last-output-end (point-max))
          (insert input))
      (user-error (substitute-command-keys "\
You don't want to do \"\\[al/eshell-send-input-maybe]\" here"))))
  (eshell-send-input))


;;; History

(require 'em-hist)

;;;###autoload
(defun al/eshell-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
Unlike `eshell-previous-matching-input-from-input', the matching
input is not forced to begin with the current input."
  (interactive "p")
  (unless (memq last-command '(al/eshell-previous-matching-input-from-input
                               al/eshell-next-matching-input-from-input))
    ;; Starting a new search.
    (setq eshell-matching-input-from-input-string
          (buffer-substring (save-excursion (beginning-of-line) (point))
                            (point))
          eshell-history-index nil))
  (eshell-previous-matching-input
   (regexp-quote eshell-matching-input-from-input-string)
   arg))

;;;###autoload
(defun al/eshell-next-matching-input-from-input (arg)
  "Search forwards through input history for match for current input."
  (interactive "p")
  (al/eshell-previous-matching-input-from-input (- arg)))

(provide 'al-eshell)

;;; al-eshell.el ends here
