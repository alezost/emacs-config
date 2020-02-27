;;; al-calendar.el --- Additional functionality for calendar, diary, etc.

;; Copyright © 2014–2016, 2018, 2020 Alex Kost

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

(require 'calendar)
(require 'solar)
(require 'al-misc)

(defvar al/calendar-date-display-form calendar-date-display-form
  "Variable used in `al/diary-insert-entry'.")

;;;###autoload
(defun al/diary-insert-entry (arg &optional event)
  "Replacement for `diary-insert-entry'.
Use `al/calendar-date-display-form' for inserted entry instead
of `calendar-date-display-form'."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let ((calendar-date-display-form al/calendar-date-display-form))
    (diary-insert-entry arg event)))

;;;###autoload
(defun al/diary-file ()
  "Visit `diary-file'."
  (interactive)
  (find-file diary-file))

(defun al/diary-date (month day year &optional mark)
  "Specific date diary entry.
This is the same as `diary-date' but allows non-positive number
for DAY.  Zero means the last day of MONTH, -1 means the last but
one day, etc."
  (let ((ddate (diary-make-date month day year)))
    (let ((dd (calendar-extract-day   ddate))
          (mm (calendar-extract-month ddate))
          (yy (calendar-extract-year  ddate))
          (m  (calendar-extract-month date))
          (y  (calendar-extract-year  date))
          (d  (calendar-extract-day   date)))
      (and
       (or (eq yy t)
           (and (listp yy) (memq y yy))
           (= y yy))
       (or (eq mm t)
           (and (listp mm) (memq m mm))
           (= m mm))
       (or (eq dd t)
           (and (listp dd) (memq d dd))
           (= d (if (> dd 0)
                    dd
                  (+ dd (calendar-last-day-of-month m y)))))
       (cons mark entry)))))


;;; Sunrise, sunset

(defun al/solar-time-string (&optional type)
  "Return time string of today's sunrise or sunset.
TYPE should be a symbol `sunrise' (default) or `sunset'."
  (if (and calendar-latitude
           calendar-longitude)
      (let* ((solar-data (solar-sunrise-sunset (calendar-current-date)))
             (solar-time (funcall (if (eq type 'sunset) #'cadr #'car)
                                  solar-data)))
        (concat (format-time-string "%F ")
                (apply #'solar-time-string solar-time)))
    (al/warning "\
Set `calendar-latitude' and `calendar-longitude' at first!")))

(defun al/solar-time (&optional type)
  "Return time value of today's sunrise or sunset.
See `al/solar-time-string' for the meaning of TYPE."
  (date-to-time (al/solar-time-string type)))

(provide 'al-calendar)

;;; al-calendar.el ends here
