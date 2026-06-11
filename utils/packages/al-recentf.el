;;; al-recentf.el --- Additional functionality for `recentf'  -*- lexical-binding: t -*-

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

(require 'recentf)

(defvar al/recentf-loaded-p nil
  "Non-nil if `recentf-load-list' is already called.")

;;;###autoload
(define-minor-mode al/recentf-mode
  "Replacement for `recentf-mode'."
  :global t
  :group 'recentf
  (unless al/recentf-loaded-p
    (recentf-load-list)
    (setq al/recentf-loaded-p t))
  (let ((hook-fun (if al/recentf-mode #'add-hook #'remove-hook)))
    (funcall hook-fun 'find-file-hook #'recentf-track-opened-file)))

(provide 'al-recentf)

;;; al-recentf.el ends here
