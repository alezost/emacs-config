;;; al-pp.el --- Additional functionality for pp

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

(defun al/pp-enable-undo (_expression buffer-name)
  "Enable undo in `buffer-name'.
This function is intended to be used as an 'after' advice for
`pp-display-expression'."
  ;; `pp-display-expression' uses `with-output-to-temp-buffer' which
  ;; disables undo by setting `buffer-undo-list' to t.
  (with-current-buffer buffer-name
    (setq buffer-undo-list nil)))

(provide 'al-pp)

;;; al-pp.el ends here
