;;; al-magit.el --- Additional functionality for magit  -*- lexical-binding: t -*-

;; Copyright © 2015–2025 Alex Kost

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

(require 'magit-diff)
(require 'git-commit)
(require 'al-buffer)

(defun al/magit-buffers (&optional type)
  "Return a list of TYPE magit buffers.
TYPE can be one of the following symbols:
  `all' (default) - all magit buffers,
  `status' - only status buffers."
  (let ((mode (if (eq type 'status)
                  'magit-status-mode
                'magit-mode)))
    (seq-filter (lambda (buf)
                  (with-current-buffer buf
                    (derived-mode-p mode)))
                (buffer-list))))

;;;###autoload
(defun al/magit-switch-buffer (&optional all)
  "Prompt for a magit status buffer and switch to it.
If ALL is non-nil, select from all magit buffers, not only statuses."
  (interactive "P")
  (al/switch-buffer "Magit buffer: "
                    :buffers (al/magit-buffers (if all 'all 'status))))

;;;###autoload
(defun al/git-commit-co-authored (name mail)
  "Insert a header acknowledging that you have co-authored the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Co-authored-by" name mail))

;;;###autoload
(defun al/magit-show-commit (commit)
  "Like `magit-show-commit' but always prompt for COMMIT."
  (interactive (list (magit-read-branch-or-commit "Show commit")))
  (magit-show-commit commit))

(provide 'al-magit)

;;; al-magit.el ends here
