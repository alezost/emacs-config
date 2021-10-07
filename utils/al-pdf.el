;;; al-pdf.el --- Additional functionality for pdf-tools

;; Copyright © 2021 Alex Kost

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
;; along with this program.  If not, see <http://www.gnu.pdf/licenses/>.

;;; Code:

(require 'pdf-view)

;;;###autoload
(defun al/pdf-view-next-page (&optional n)
  "Move N pages forward.
This is similar to `pdf-view-next-page-command' but it overlaps
the last page."
  (interactive "p")
  (or n (setq n 1))
  (let* ((last-page (pdf-cache-number-of-pages))
         (page (mod (+ (pdf-view-current-page) n)
                    last-page))
         (page (if (= 0 page) last-page page)))
    (pdf-view-goto-page page)))

;;;###autoload
(defun al/pdf-view-previous-page (&optional n)
  "Move N pages backwards.
This is similar to `pdf-view-next-page-command' but it overlaps
the first page."
  (interactive "p")
  (al/pdf-view-next-page (- n)))

(provide 'al-pdf)

;;; al-pdf.el ends here
