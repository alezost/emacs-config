;;; al-thingatpt.el --- Additional functionality for `thingatpt'  -*- lexical-binding: t -*-

;; Copyright © 2025 Alex Kost

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

(require 'thingatpt)

(defvar al/youtube-video-at-point-regexp
  "[-_0-9a-zA-Z]\\{11\\}"
  "Regular expression matching youtube video ID.")

(defvar al/youtube-playlist-at-point-regexp
  "PL[-_0-9a-zA-Z]+"
  "Regular expression matching youtube playlist ID.")

(defun al/youtube-at-point-bounds ()
  "Return the bounds of youtube video ID at point."
  (save-excursion
    (when (or (thing-at-point-looking-at al/youtube-playlist-at-point-regexp)
              (thing-at-point-looking-at al/youtube-video-at-point-regexp))
      (cons (match-beginning 0) (match-end 0)))))

(put 'youtube 'bounds-of-thing-at-point 'al/youtube-at-point-bounds)

(provide 'al-thingatpt)

;;; al-thingatpt.el ends here
