;;; al-text.el --- Additional functionality related to text editing  -*- lexical-binding: t -*-

;; Copyright © 2014–2025 Alex Kost

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

(defun al/replace (from-string to-string &optional start end)
  "This is similar to `query-replace' but without querying.

Replace some occurrences of FROM-STRING with TO-STRING in the
current buffer.

START and END specify the region to operate on.  If they are not
specified, then the currenty marked region is used.  If there is
no marked region, then the whole buffer is used."
  (let* ((count 0)
         (regionp (use-region-p))
         (from-re (regexp-quote from-string))
         (str-diff (- (length to-string)
                      (length from-string)))
         (beg (cond
               (start start)
               (regionp (region-beginning))))
         (end (cond
               (end end)
               (regionp (region-end)))))
    (save-excursion
      (when beg (goto-char beg))
      (while (re-search-forward from-re end t)
        (replace-match to-string)
        (setq end (+ end str-diff) ; extend/shrink the end bound after replacing
              count (1+ count))))
    (unless (= 0 count)
      (message "'%s' has been replaced with '%s' %d time(s)."
               from-string to-string count))))

(defun al/shorten-string (string length)
  "Shorten STRING to make it no longer than LENGTH."
  (if (<= (length string) length)
      string
    (concat (substring string 0 (- length 1))
            "…")))

(defvar al/check-parens-modes
  '(lisp-data-mode scheme-mode)
  "List of parent modes where `al/check-parens' is called.")

(defun al/check-parens ()
  "Run `check-parens' if `major-mode' derived from `al/check-parens-modes'."
  (when (derived-mode-p al/check-parens-modes)
    (check-parens)))

(provide 'al-text)

;;; al-text.el ends here
