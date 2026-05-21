;;; al-format.el --- Additional functionality related to formatting  -*- lexical-binding: t -*-

;; Copyright © 2021–2026 Alex Kost

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

;;; Commentary:

;; This file provides some utilities for converting text to various
;; things (numbers, time, etc.) and inverse.

;;; Code:

(eval-when-compile (require 'cl-lib))


;;; Formatting time

(defun al/time-string-to-seconds (str)
  ;; This function originates from `org-emms-time-string-to-seconds'
  ;; (from `org-emms' package).
  "Convert timestring STR to a number of seconds.
STR can have one of the following formats:
- SS
- MM:SS
- HH:MM:SS
"
  (save-match-data
    (if (string-match "\\([0-9]+:\\)?\\([0-9]+\\):\\([0-9]+\\)" str)
	(let ((h (if (match-beginning 1)
                     (string-to-number (match-string 1 str))
                   0))
	      (m (string-to-number (match-string 2 str)))
	      (s (string-to-number (match-string 3 str))))
	  (+ (* h 3600) (* m 60) s))
      (string-to-number str))))


;;; Formatting bytes

(defvar al/format-byte-alist
  '((1e9 "G" alect-color-level-3)
    (1e6 "M" alect-color-level-2)
    (1e3 "k" alect-color-level-1)
    (nil "b" shadow))
  "Internal variable for `al/format-bytes'.")

(defun al/format-bytes-1 (bytes)
  "Return (NUM UNIT) list to format BYTES."
  (let ((rest al/format-byte-alist)
        (num nil)
        (border nil))
    (while (if (setq border (caar rest))
               (> 1 (setq num (/ (float bytes) border)))
             (setq num bytes)
             nil)
      (setq rest (cdr rest)))
    (let ((assoc (car rest)))
      (list num (propertize (nth 1 assoc) 'face (nth 2 assoc))))))

(defun al/format-bytes (bytes &optional width)
  "Return human readable string from BYTES.
Result has WIDTH length plus 1 character for unit."
  (or (>= bytes 0)
      (error "BYTES should be a non-negative number"))
  (cl-multiple-value-bind (num unit)
      (al/format-bytes-1 bytes)
    (let* ((width-str (and width (number-to-string width)))
           (fmt (if (or (string= "b" unit)
                        (and width
                             (>= num (expt 10 (- width 2)))))
                    (concat "%" width-str "d")
                  (concat "%" width-str ".1f"))))
      (concat (format fmt num) unit))))

(provide 'al-format)

;;; al-format.el ends here
