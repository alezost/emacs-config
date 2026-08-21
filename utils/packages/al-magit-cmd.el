;;; al-magit-cmd.el --- Additional commands for `magit' package  -*- lexical-binding: t -*-

;; Copyright © 2015–2026 Alex Kost

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

;; This file contains "entry point" commands for `magit' package to
;; avoid recursive loading.  See `al-eshell-cmd' commentary for details.

;;; Code:

(require 'magit)
(require 'al-buffer)
(require 'al-magit)

;;;###autoload
(defun al/magit-switch-buffer (&optional all)
  "Switch to the next magit status buffer.
If ALL is non-nil, select from all magit buffers, not only statuses."
  (interactive "P")
  (al/rotate-or-select-buffer
   (al/magit-buffers (if all 'all 'status))
   #'magit-status
   (when all "Magit buffer: "))
  (set-transient-map al/magit-switch-map))

;;;###autoload
(defun al/magit-show-commit (commit)
  "Like `magit-show-commit' but always prompt for COMMIT."
  (interactive (list (magit-read-branch-or-commit "Show commit")))
  (magit-show-commit commit))

(provide 'al-magit-cmd)

;;; al-magit-cmd.el ends here
