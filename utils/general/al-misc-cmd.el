;;; al-misc-cmd.el --- Miscellaneous interactive commands  -*- lexical-binding: t -*-

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
(require 'al-buffer)

(defvar org-link-plain-re)

;;;###autoload
(defun al/next-link (&optional search-backward)
  "Go to the next link."
  ;; This is a simplified version of `org-next-link'.
  (interactive)
  (require 'ol)
  (let ((pos (point))
	(search-fun (if search-backward
                        #'re-search-backward
                      #'re-search-forward)))
    (when (looking-at org-link-plain-re)
      ;; Don't stay stuck at the current link.
      (forward-char (if search-backward -1 1)))
    (if (funcall search-fun org-link-plain-re nil t)
	(goto-char (match-beginning 0))
      (goto-char pos)
      (message "No further link found"))))

;;;###autoload
(defun al/previous-link ()
  "Go to the previous link."
  (interactive)
  (al/next-link t))

;;;###autoload
(defun al/next-error (&optional num reset)
  "Like `next-error' but display the popped buffer in current window.
NUM is an integer specifying how many error messages to move.
If RESET is non-nil (interactively, with prefix argument), restart
error messages from the beginning."
  (interactive (list 1 current-prefix-arg))
  (when-let ((buffer (next-error-find-buffer)))
    (with-current-buffer buffer
      ;; With `occur', it is possible to show popped buffer in the
      ;; current window but with `grep' and other compilation-like
      ;; buffers, it is not possible because `compilation-goto-locus'
      ;; (eventually called by `next-error') pops the grep buffer at
      ;; first and then switches to the target error buffer.
      (if (eq next-error-function 'compilation-next-error-function)
          (funcall next-error-function num reset)
        (al/with-pop-to-current-window
          (funcall next-error-function num reset)))
      (next-error-found buffer (current-buffer))
      (message "%s locus from %s"
               (cond (reset      "First")
                     ((eq num 0) "Current")
                     ((< num 0)  "Previous")
                     (t          "Next"))
               next-error-last-buffer))))

;;;###autoload
(defun al/previous-error ()
  "Like `previous-error' but display the popped buffer in current window."
  (interactive)
  (al/next-error -1))

;;;###autoload
(defun al/create-tags (shell-cmd)
  "Create tags file using shell command SHELL-CMD.
Interactively prompt for shell command.
With prefix, prompt for directory as well."
  (interactive
   (let ((dir (if current-prefix-arg
                  (read-directory-name "Root tags directory: ")
                "")))
     (list (read-shell-command
            "Shell command for generating tags: "
            (format "find %s -type f -name '*.[ch]' | etags -" dir)))))
  (eshell-command shell-cmd))

;; Idea from <http://www.emacswiki.org/emacs-en/DisabledCommands>.
;;;###autoload
(defun al/show-disabled-commands ()
  "Show all disabled commands."
  (interactive)
  (with-output-to-temp-buffer "*Disabled commands*"
    (mapatoms (lambda (symbol)
                (when (get symbol 'disabled)
                  (prin1 symbol)
                  (princ "\n"))))))

;;;###autoload
(defun al/save-everything ()
  "Save everything that should be saved."
  (interactive)
  (save-some-buffers 'no-ask)
  (with-no-warnings
    (and (featurep 'emms)
         (al/emms-save-playlists))
    (and (featurep 'saveplace)
         save-place-loaded
         (progn
           (save-places-to-alist)
           (save-place-alist-to-file)))
    (and (featurep 'recentf)
         (memq 'recentf-track-opened-file find-file-hook)
         (recentf-save-list))))


;;; Highlighting of the current line

(defvar al/hl-line-mode-exclude '(grep-mode)
  "List of modes where `al/hl-line-mode' does nothing.")

;;;###autoload
(defun al/hl-line-mode ()
  "Toggle `hl-line-mode' maybe.
Do nothing if current mode is derived from `al/hl-line-mode-exclude'."
  (interactive)
  (unless (derived-mode-p al/hl-line-mode-exclude)
    (hl-line-mode 'toggle)))


;;; Checking parentheses

(defvar al/check-parens-modes
  '(lisp-data-mode scheme-mode)
  "List of parent modes where `al/check-parens' is called.")

;;;###autoload
(defun al/check-parens ()
  "Run `check-parens' if `major-mode' derived from `al/check-parens-modes'."
  (interactive)
  (when (derived-mode-p al/check-parens-modes)
    (check-parens)))

(provide 'al-misc-cmd)

;;; al-misc-cmd.el ends here
