;;; al-url.el --- Code for searching and downloading various stuff  -*- lexical-binding: t -*-

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

(require 'wget)

(defvar al/url-mp3-re
  (rx "http" (? ?s) "://" (1+ any) ".mp3")
  "Regexp for mp3 file.")

;;;###autoload
(defun al/url-wget-mp3 (url)
  "Download the first mp3 file from URL with `wget'."
  (interactive
   (list (read-string "Download mp3 from URL: "
                      (thing-at-point 'url))))
  (let* ((buf (url-retrieve-synchronously url))
         (mp3 (with-current-buffer buf
                (re-search-forward al/url-mp3-re)
                (match-string 0))))
    (when (y-or-n-p (format "Download '%s'? " mp3))
      (wget mp3))))

(provide 'al-url)

;;; al-url.el ends here
