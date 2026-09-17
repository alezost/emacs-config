;;; al-shr.el --- Additional functionality for `shr' package  -*- lexical-binding: t -*-

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

(require 'shr)

(declare-function eww-follow-link "eww")

(defun al/shr-browse-this-url (&optional arg)
  "Browse URL at point guessing a suitable browser.
With ARG, use default browser."
  (interactive "P")
  (if (or arg
          (not (derived-mode-p 'eww-mode)))
      (browse-url (shr-url-at-point nil))
    (eww-follow-link)))

(provide 'al-shr)

;;; al-shr.el ends here
