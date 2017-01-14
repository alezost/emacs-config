;;; al-ivy.el-- Additional functionality for ivy-mode

;; Copyright © 2017 Alex Kost

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

(require 'ivy)

(defvar al/ivy-format-selected "─► ")
(defvar al/ivy-format-other "   ")

(defun al/ivy-format-function (candidates)
  "Transform CANDIDATES into a string for minibuffer.
This function is suitable for `ivy-format-function'."
  (ivy--format-function-generic
   (lambda (str)
     (concat al/ivy-format-selected
             (propertize str 'face 'ivy-current-match)))
   (lambda (str)
     (concat al/ivy-format-other str))
   candidates
   "\n"))

(provide 'al-ivy)

;;; al-ivy.el ends here
