;;; al-pdf.el --- Additional functionality for pdf-tools  -*- lexical-binding: t -*-

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


;;; Copying text with mouse

(defun al/pdf-view-kill-ring-save ()
  "Copy the current region to the `kill-ring'.
This is similar to `pdf-view-kill-ring-save' but without
deactivating the region."
  (interactive)
  (when (pdf-view-active-region-p)
    (let ((text (mapconcat 'identity (pdf-view-active-region-text) "\n")))
      (kill-new text)
      (message "Copied: «%s»" text))))

(defun al/pdf-view-select-region (event)
  "Select a region and copy it to `kill-ring'."
  (interactive "@e")
  (pdf-view-mouse-set-region event)
  (al/pdf-view-kill-ring-save))

;; Originates from
;; <https://emacs.stackexchange.com/questions/52457/select-a-word-in-a-pdf-by-double-clicking-on-it-with-pdf-tools>
(defun al/pdf-view-select-word (event)
  "Select word at mouse and copy it to `kill-ring'."
  (interactive "@e")
  (let* ((posn (event-start event))
         (xy (posn-object-x-y posn))
         (size (pdf-view-image-size))
         (page (pdf-view-current-page))
         (x (/ (car xy) (float (car size))))
         (y (/ (cdr xy) (float (cdr size)))))
    (setq pdf-view-active-region
          (pdf-info-getselection page (list x y x y) 'word))
    (pdf-view-display-region pdf-view-active-region)
    (al/pdf-view-kill-ring-save)))

(defun al/pdf-view-deactivate-region ()
  "Deactivate the region but do not redisplay the pages.
This is similar to `pdf-view-deactivate-region' but better."
  (interactive)
  (when pdf-view-active-region
    (setq pdf-view-active-region nil)
    (deactivate-mark)))

(provide 'al-pdf)

;;; al-pdf.el ends here
