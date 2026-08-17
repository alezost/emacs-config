;;; time.el --- Time, calendar, diary, appointments, notifications, …  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

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

(eval-when-compile
  (require 'al-aux-macros))
(require 'al-places)
(require 'al-general)
(require 'al-key)


;;; Global keys

(al/bind-key* "M-T" tui/notification)
(al/bind-keys
 :prefix-map al/calendar-map
 :prefix-doc "Map for calendar, diary, notifications, etc."
 :prefix-key "M-C"
 ("M-C" . calendar)
 ("c"   . calendar)
 ("d"   . diary)
 ("D"   . al/diary-file)
 ("A"   . appt-activate)
 ("a n" . appt-add)
 ("a k" . appt-delete))


;;; Misc settings and packages

(al/eval-after-load time
  (setq
   display-time-interval 5
   display-time-format " %H:%M:%S"))

;; `calendar-date-style' is used for other variables.
(al/setq-no-warnings calendar-date-style 'iso)

(al/eval-after-load calendar
  (al/load-settings "calendar"))

(al/eval-after-load timer-list
  (al/load-settings "timer-list"))

(al/eval-after-load appt
  (al/load-settings "appt"))

(al/eval-after-load al-notification
  (al/setq-file
   al/notification-sound (al/sound-dir-file "alarm.wav")))

;;; time.el ends here
