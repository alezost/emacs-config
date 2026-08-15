;;; al-guix.el --- Additional functionality for Guix  -*- lexical-binding: t -*-

;; Copyright © 2015-2016 Alex Kost

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

(require 'al-buffer)

(declare-function guix-package-info-buffer-name "guix-ui-package")
(declare-function guix-package-list-buffer-name "guix-ui-package")
(declare-function guix-generation-info-buffer-name "guix-ui-generation")
(declare-function guix-generation-list-buffer-name "guix-ui-generation")

(defvar guix-current-profile)

;;;###autoload
(defun al/guix-commit-url (commit)
  "Put to `kill-ring' and browse guix git repo URL for COMMIT."
  (interactive "sGuix commit: ")
  (let ((url (concat "http://git.savannah.gnu.org/cgit/guix.git/commit/?id="
                     commit)))
    (kill-new url)
    (browse-url url)))

;;;###autoload
(defun al/guix-switch-to-package-info-buffer ()
  (interactive)
  (al/display-buffer (guix-package-info-buffer-name
                      guix-current-profile)))

;;;###autoload
(defun al/guix-switch-to-generation-info-buffer ()
  (interactive)
  (al/display-buffer (guix-generation-info-buffer-name
                      guix-current-profile)))

;;;###autoload
(defun al/guix-switch-to-package-list-buffer ()
  (interactive)
  (al/display-buffer (guix-package-list-buffer-name
                      guix-current-profile)))

;;;###autoload
(defun al/guix-switch-to-generation-list-buffer ()
  (interactive)
  (al/display-buffer (guix-generation-list-buffer-name
                      guix-current-profile)))

(provide 'al-guix)

;;; al-guix.el ends here
