;;; al-magit-popup.el --- Additional functionality for magit-popup library

;; Copyright © 2016 Alex Kost

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

(require 'magit-popup)

;; This "previous popup" functionality is hardcoded in
;; 'magit-invoke-popup-action' to handle "q" key, but I want to bind it
;; to another key.  I didn't make a pull request to add
;; 'magit-popup-previous-or-quit' and to bind it to "q" in
;; 'magit-popup-mode-map' because it would be undesired, if I understood
;; it right from the commit message of
;; <https://github.com/magit/magit/commit/2ef07e3aa01b19b68e348ee4c4ed9e3428ef4e1e>.
;;;###autoload
(defun al/magit-popup-previous-or-quit ()
  "Quit the current popup and return to the previous one if possible."
  (interactive)
  (magit-popup-quit)
  (when magit-previous-popup
    (magit-popup-mode-setup magit-previous-popup nil)))

(provide 'al-magit-popup)

;;; al-magit-popup.el ends here
