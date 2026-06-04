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

(require 'al-places)
(require 'al-general)
(require 'al-key)


;;; Global keys

(al/bind-key "M-T" al/notification-tui)
(al/bind-keys
 :prefix-map al/calendar-map
 :prefix-docstring "Map for calendar, diary, notifications, etc."
 :prefix "M-C"
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

(al/setq-no-warnings calendar-date-style 'iso)
(al/eval-after-load calendar
  (setq
   diary-file (al/notes-dir-file "diary")
   calendar-week-start-day 1
   calendar-date-display-form '(dayname ", " day " " monthname " " year)
   calendar-mark-diary-entries-flag t)

  (al/bind-keys
   :map calendar-mode-map
   ("t"   . calendar-goto-today)
   ("o"   . calendar-backward-day)
   ("u"   . calendar-forward-day)
   ("."   . calendar-backward-week)
   ("e"   . calendar-forward-week)
   ("z"   . calendar-unmark)
   ("l"   . holidays)
   ("C-a" . calendar-beginning-of-week)
   ("<ctrl-i>" . calendar-end-of-week)
   ("M-o" . calendar-backward-month)
   ("M-u" . calendar-forward-month)
   ("M-." . calendar-scroll-right-three-months)
   ("M-e" . calendar-scroll-left-three-months)
   ("M-A" . calendar-beginning-of-month)
   ("M-I" . calendar-end-of-month)
   ("H-." . calendar-backward-year)
   ("H-e" . calendar-forward-year)
   ("n"   . al/diary-insert-entry)
   ("i d" . al/diary-insert-entry))

  ;; Do not ruin the mode-line.
  (setq calendar-mode-line-format nil)

  (al/add-hook-maybe 'calendar-mode-hook 'al/bar-cursor-type)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(al/eval-after-load al-calendar
  (setq al/calendar-date-display-form
        '((format "%s %.3s %2s" year monthname day))))

(al/eval-after-load solar
  (setq
   calendar-latitude 50.6
   calendar-longitude 36.6
   calendar-location-name "home"
   calendar-time-display-form
   '(24-hours ":" minutes
     (if time-zone " (") time-zone (if time-zone ")"))))

(al/eval-after-load diary-lib
  (setq
   diary-number-of-entries 3
   diary-comment-start "#")
  (al/require al-calendar)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t))

(al/eval-after-load timer-list
  (al/bind-keys
    :map timer-list-mode-map
    ("k"   . timer-list-cancel)
    ("C-k" . timer-list-cancel)))

(al/eval-after-load appt
  (setq
   appt-audible nil
   appt-display-diary nil
   appt-message-warning-time 5
   appt-display-interval 1)
  (al/require al-appt))

(al/eval-after-load al-appt
  (advice-add 'appt-display-message :override #'al/appt-display-message)
  (advice-add 'appt-mode-line :override #'al/appt-mode-line)

  (when (al/require al-file)
    (al/setq-file
     al/appt-notify-normal-sound (al/sound-dir-file "drums.wav")
     al/appt-notify-urgent-sound (al/sound-dir-file "bell.oga"))))

(al/eval-after-load al-notification
  (when (al/require al-file)
    (al/setq-file
     al/notification-sound (al/sound-dir-file "alarm.wav"))))

;;; time.el ends here
