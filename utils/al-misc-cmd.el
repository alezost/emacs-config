;;; al-misc-cmd.el --- Miscellaneous interactive commands  -*- lexical-binding: t -*-

;; Copyright © 2013–2016, 2019–2020 Alex Kost

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

(require 'org)

;;;###autoload
(defun al/next-link (&optional search-backward)
  "Go to the next link."
  ;; The function is almost the same as `org-next-link'.
  (interactive)
  (when (and org-link--search-failed
             (eq this-command last-command))
    (goto-char (point-min))
    (message "Link search wrapped back to beginning of buffer"))
  (setq org-link--search-failed nil)
  (let* ((pos (point))
	 (srch-fun (if search-backward
                       're-search-backward
                     're-search-forward)))
    (when (looking-at org-link-any-re)
      ;; Don't stay stuck at link without an org-link face.
      (forward-char (if search-backward -1 1)))
    (if (funcall srch-fun org-link-any-re nil t)
	(progn
	  (goto-char (match-beginning 0))
	  (when (outline-invisible-p) (org-fold-show-context)))
      (goto-char pos)
      (setq org-link--search-failed t)
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
(defun al/set-isearch-input-method (&optional input-method)
  "Activate input method INPUT-METHOD in interactive search.
See `set-input-method' for details."
  (set-input-method input-method)
  (setq isearch-input-method-function input-method-function)
  (isearch-update))

(provide 'al-misc-cmd)

;;; al-misc-cmd.el ends here
