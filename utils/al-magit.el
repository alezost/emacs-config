;;; al-magit.el --- Additional functionality for magit

;; Copyright © 2015–2017 Alex Kost

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

(require 'git-commit)
(require 'al-buffer)

;;;###autoload
(defun al/magit-switch-buffer ()
  "Switch to a magit status buffer preferably using `ivy-read'."
  (interactive)
  (al/switch-buffer "Magit buffer: "
                    :initial-input "*magit: "))

;;;###autoload
(defun al/git-commit-co-authored (name mail)
  "Insert a header acknowledging that you have co-authored the commit."
  (interactive (git-commit-self-ident))
  (git-commit-insert-header "Co-authored-by" name mail))

(provide 'al-magit)

;;; al-magit.el ends here
