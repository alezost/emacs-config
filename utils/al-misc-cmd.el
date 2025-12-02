;;; al-misc-cmd.el --- Miscellaneous interactive commands  -*- lexical-binding: t -*-

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
(defun al/refontify (&rest _)
  "Refontify the current buffer."
  (jit-lock-refontify))


;;; Spelling and languages

;;;###autoload
(defun al/set-input-method (&optional input-method)
  "Activate input method INPUT-METHOD for the current buffer.
This is the same as `set-input-method', except it also handles
`isearch'."
  (interactive
   (let ((default (or (car input-method-history)
                      default-input-method)))
     (list (read-input-method-name
	    (format-prompt "Set input method" default)
	    default))))
  (set-input-method input-method)
  (when isearch-mode
    (setq isearch-input-method-function input-method-function)
    (isearch-update)))

(provide 'al-misc-cmd)

;;; al-misc-cmd.el ends here
