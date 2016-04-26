;;; al-comint.el --- Additional functionality for comint

;; Copyright © 2015-2016 Alex Kost

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

(require 'comint)

;;;###autoload
(defun al/comint-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
Unlike `comint-previous-matching-input-from-input', the matching
input is not forced to begin with the current input."
  (interactive "p")
  (unless (memq last-command '(al/comint-previous-matching-input-from-input
                               al/comint-next-matching-input-from-input))
    ;; Starting a new search.
    (setq comint-matching-input-from-input-string
          (buffer-substring
           (or (marker-position comint-accum-marker)
               (process-mark (get-buffer-process (current-buffer))))
           (point))
          comint-input-ring-index nil))
  (comint-previous-matching-input
   (regexp-quote comint-matching-input-from-input-string)
   arg))

;;;###autoload
(defun al/comint-next-matching-input-from-input (arg)
  "Search forwards through input history for match for current input."
  (interactive "p")
  (al/comint-previous-matching-input-from-input (- arg)))

(provide 'al-comint)

;;; al-comint.el ends here
