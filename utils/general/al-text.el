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

(require 'seq)
(require 'let-macros)
(require 'al-misc)

(defun al/shorten-string (string length)
  "Shorten STRING to make it no longer than LENGTH."
  (if (<= (length string) length)
      string
    (concat (substring string 0 (- length 1))
            "…")))

(defun al/first-line (string)
  "Return the first line of STRING."
  (if-let ((end (string-match "\n" string)))
      (substring string 0 end)
    string))

(defun al/string-candidates (&optional thing filters)
  "Return list of strings from various places (clipboard, kill ring, etc.)

If THING is non-nil, `thing-at-point' with this THING is also one of the
candidates.

FILTERS is a list of functions, (F1 F2 ... FN), applied to each
candidate.  FILTERS are called from left to right, passing result to the
next function i.e., (FN (... (F2 (F1 ELEMENT)))).  If any filter returns
nil, this candidate is removed from the final list."
  (let ((filters (cons #'substring-no-properties filters))
        (candidates (list
                     (and thing (thing-at-point thing))
                     (gui--selection-value-internal 'CLIPBOARD)
                     (gui--selection-value-internal 'PRIMARY)
                     (car kill-ring))))
    (seq-keep (lambda (candidate)
                (al/multi-filter candidate filters))
              (seq-uniq (delq nil candidates)))))

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
