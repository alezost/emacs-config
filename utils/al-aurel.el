;;; al-aurel.el --- Additional functionality for AURel  -*- lexical-binding: t -*-

;; Copyright © 2016 Alex Kost

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

(require 'aurel)
(require 'al-buffer)

;;;###autoload
(defun al/switch-to-aurel-list ()
  "Switch to the `aurel-list-buffer-name' buffer."
  (interactive)
  (al/switch-to-buffer-or-funcall
   aurel-list-buffer-name
   (lambda () (call-interactively 'aurel-package-search))))

;;;###autoload
(defun al/switch-to-aurel-info ()
  "Switch to the `aurel-info-buffer-name' buffer."
  (interactive)
  (al/switch-to-buffer-or-funcall
   aurel-info-buffer-name
   (lambda () (call-interactively 'aurel-package-info))))

(provide 'al-aurel)

;;; al-aurel.el ends here
