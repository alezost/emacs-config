;;; al-browse-at-remote.el --- Additional functionality for `browse-at-remote'  -*- lexical-binding: t -*-

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

(require 'browse-at-remote)
(require 'magit-section)
(require 'magit-git)
(require 'al-misc)

(defun al/browse-at-remote-get-url (fun &rest args)
  "Return URL with full commit hash to browse.
This function is intended to be used as an `around' advice for
`browse-at-remote-get-url'."
  (if-let* ((right-mode (derived-mode-p '(magit-log-mode
                                          magit-status-mode)))
            (section (magit-current-section))
            (value (oref section value)))
      (browse-at-remote--commit-url (magit-rev-parse value))
    (apply fun args)))

;;;###autoload
(defun al/browse-at-remote-kill ()
  "Add the current `browse-at-remote' URL to the kill ring.
This is the same as `browse-at-remote-kill' except it also shows the
killed URL in echo area."
  (interactive)
  (al/with-eval-to-kill-ring
    (browse-at-remote-get-url)))

(provide 'al-browse-at-remote)

;;; al-browse-at-remote.el ends here
