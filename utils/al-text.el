;;; al-text.el --- Additional functionality related to text editing  -*- lexical-binding: t -*-

;; Copyright © 2025–2026 Alex Kost

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

(defun al/shorten-string (string length)
  "Shorten STRING to make it no longer than LENGTH."
  (if (<= (length string) length)
      string
    (concat (substring string 0 (- length 1))
            "…")))

(defun al/parse-ytdlp-file-name-output (string)
  "Parse and return file name from `yt-dlp' output.
STRING is the output of \"yt-dlp --print filename\" or similar command.
Return nil if string cannot be parsed."
  (if (string-match-p "ERROR" string)
      (error "Cannot parse file name output: %S" string)
    (let* ((string (substring-no-properties string))
           (file-name (string-trim-right string "\n")))
      ;; Something probably went wrong if the name is so long.
      (al/shorten-string file-name 256))))

(provide 'al-text)

;;; al-text.el ends here
