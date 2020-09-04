;;; al-org.el --- Additional functionality for org-mode

;; Copyright © 2012–2016, 2018–2020 Alex Kost

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

(require 'cl-lib)
(require 'al-text-cmd)
(require 'org)
(require 'org-table)

(defvar al/org-link-no-description-regexp
  (rx string-start (or "file" "emms") ":")
  "Regexp for `al/org-set-link-description'.")

(defun al/org-link-set-description (fun link &optional description)
  "Call FUN with LINK and fixed DESCRIPTION.
This function is intended to be used as an 'around' advice for
`org-make-link-string':

  (advice-add 'org-link-make-string
              :around #'al/org-link-set-description)

If `al/org-link-no-description-regexp' matches LINK or if
DESCRIPTION is the same as LINK, then description is ignored (FUN
is called with LINK only)."
  (if (or (string-match-p al/org-link-no-description-regexp link)
          (and description
               (string= link description)))
      (funcall fun link)
    (funcall fun link description)))

(defun al/org-get-time-stamp (time &optional with-hm)
  "Return org time stamp string from TIME (iso or system format).
WITH-HM means use the stamp format that includes the time of the day."
  (let ((fmt (funcall (if with-hm 'cdr 'car)
                      org-time-stamp-formats)))
    (and (stringp time)
         (setq time (org-read-date nil t time)))
    (format-time-string fmt time)))

(defun al/org-get-time-from-stamp (org-time &optional end-time-p force)
  "Return time value from org time stamp or range ORG-TIME.
Use the start part of the time range if END-TIME-P is nil.
If ORG-TIME is a single time-stamp and END-TIME-P is non-nil,
return nil; with FORCE return its time value. "
  (or (string-match org-tsr-regexp org-time)
      (error "Wrong org time stamp/range"))
  (if (string-match "---?" org-time)
      (setq org-time
            (if end-time-p
                (substring org-time (match-end 0))
              (substring org-time 0 (match-beginning 0))))
    (and end-time-p (not force)
         (setq org-time nil)))
  (and org-time
       (eval (cons 'encode-time
                   (org-parse-time-string org-time)))))

(defun al/org-return-indent ()
  "Insert a new row in tables, insert a newline and indent otherwise."
  (interactive)
  (if (org-at-table-p)
      (org-table-insert-row t)
    (org-return t)))


;;; Tables

(defun al/org-table-beginning-of-section ()
  "Move point to beginning of current section (a space between
horizontal lines) - behaviour is similar to `backward-word' or
`org-table-beginning-of-field'."
  (interactive)
  (let ((cur-col (current-column))
	(beg (org-table-begin)))
    ;; position a point on a proper line
    (if (re-search-backward org-table-hline-regexp beg t)
	(forward-line)
      (org-table-goto-line 1))
    (move-to-column cur-col)))

(defun al/org-table-next-column ()
  "Move point to first row, next column of the current section"
  (interactive)
  (al/org-table-beginning-of-section)
  (org-table-next-field))

(defun al/org-table-kill-rows-recalculate ()
  "Kill all empty rows in the current section and recalculate a
table. Emptiness is checked in the current column after the current
row."
  (interactive)
  (let ((cur-col (org-table-current-column)))
    (save-excursion
      (beginning-of-line)
      (while (and (org-at-table-p)
		  (not (looking-at org-table-hline-regexp)))
	(if (equal "" (org-table-get (org-table-current-line) cur-col))
	    (org-table-kill-row)
	  (forward-line))))
    (org-table-recalculate 'iterate 'no-align)
    (org-table-align)))

(defun al/org-table-next-table ()
  "Move point to the next org-table in the current buffer"
  (interactive)
  (beginning-of-line)
  (and (al/re-search-forward "^[^|]")
       (al/re-search-forward "^|")
       (org-table-goto-line (+ 1 (org-table-current-line)))))

(provide 'al-org)

;;; al-org.el ends here
