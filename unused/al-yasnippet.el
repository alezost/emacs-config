;;; al-yasnippet.el --- Additional functionality for yasnippet  -*- lexical-binding: t -*-

;; Copyright © 2014-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'yasnippet)

;;;###autoload
(defun al/yas-next-field-or-expand ()
  "Go to the next field if a snippet is in progress or perform an expand."
  (interactive)
  (if (yas--snippets-at-point 'all)
      (goto-char (overlay-start yas--active-field-overlay))
    (yas-expand)))

;;;###autoload
(defun al/yas-exit-and-expand ()
  "Exit all snippets and expand a snippet before point."
  (interactive)
  (save-excursion (yas-exit-all-snippets))
  (yas-expand))

(provide 'al-yasnippet)

;;; al-yasnippet.el ends here
