;;; al-shell.el --- Additional functionality for `shell' package  -*- lexical-binding: t -*-

;; Copyright © 2026 Alex Kost

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

(defun al/shell-set-comment-variables ()
  "Set comment local variables for `shell-mode'."
  ;; Taken from `sh-base-mode'.
  (setq-local
   comment-start "# "
   comment-start-skip "#+[\t ]*"))

(defun al/shell-set-local-variables ()
  "Set missing local variables for `shell-mode'."
  (al/shell-set-comment-variables))

(provide 'al-shell)

;;; al-shell.el ends here
