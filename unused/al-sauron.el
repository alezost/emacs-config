;;; al-sauron.el --- Additional functionality for sauron  -*- lexical-binding: t -*-

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

(require 'sauron)

;;;###autoload
(defun al/sauron-toggle-hide-show ()
  "Toggle between showing/hiding the sauron window or frame.
If sauron is shown, switch to its window."
  (interactive)
  (sauron-toggle-hide-show)
  (let ((buf (get-buffer-window sr-buffer)))
    (and buf (select-window buf))))

;;;###autoload
(defun al/sauron-restart ()
  "Restart sauron."
  (interactive)
  (sauron-stop)
  (sauron-start))

(provide 'al-sauron)

;;; al-sauron.el ends here
