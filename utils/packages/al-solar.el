;;; al-solar.el --- Additional functionality for `solar' package  -*- lexical-binding: t -*-

;; Copyright © 2018 Alex Kost

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

(require 'solar)
(require 'al-general)

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
    (al/warning-message "\
Set `calendar-latitude' and `calendar-longitude' at first!")))

(defun al/solar-time (&optional type)
  "Return time value of today's sunrise or sunset.
See `al/solar-time-string' for the meaning of TYPE."
  (date-to-time (al/solar-time-string type)))

(provide 'al-solar)

;;; al-solar.el ends here
