;;; al-saveplace.el --- Additional functionality for saveplace

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

(require 'saveplace)

(defun al/save-places-to-alist ()
  "Go through `buffer-list', saving places to alist.
This function is intended to be used as a substitution for
`save-places-to-alist'.  The difference is: this function does
not save positions for `dired' buffers."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when buffer-file-name
        (save-place-to-alist)))))

(provide 'al-saveplace)

;;; al-saveplace.el ends here
