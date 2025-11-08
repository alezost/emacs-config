;;; time.el --- Time, calendar, diary, appointments, notifications, …

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

(require 'al-key)


;;; Global keys

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
 ("a k" . appt-delete)
 ("M-T"   (al/timer-set (* 3 60)))
 ("T"     (al/timer-set (* 45 60)))
 ("t n" . al/timer-set)
 ("t t" . al/timer-remaining-time)
 ("t k" . al/timer-cancel))


;;; Misc settings and packages

(with-eval-after-load 'time
  (setq
   display-time-interval 5
   display-time-format " %H:%M:%S"))

(setq calendar-date-style 'iso)
(with-eval-after-load 'calendar
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

(with-eval-after-load 'al-calendar
  (setq al/calendar-date-display-form
        '((format "%s %.3s %2s" year monthname day))))

(with-eval-after-load 'solar
  (setq
   calendar-latitude 50.6
   calendar-longitude 36.6
   calendar-location-name "home"
   calendar-time-display-form
   '(24-hours ":" minutes
     (if time-zone " (") time-zone (if time-zone ")"))))

(with-eval-after-load 'diary-lib
  (setq
   diary-number-of-entries 3
   diary-comment-start "#")
  (require 'al-calendar nil t)
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t))

(with-eval-after-load 'appt
  (setq
   appt-audible nil
   appt-display-diary nil
   appt-message-warning-time 5
   appt-display-interval 1)
  (when (require 'sauron nil t)
    (add-to-list 'sauron-modules 'sauron-org))
  (when (require 'al-appt nil t)
    (advice-add 'appt-display-message :override #'al/appt-display-message)
    (advice-add 'appt-mode-line :override #'al/appt-mode-line)))
(al/eval-after-init
  (when (string= server-name "server-emms")
    (appt-activate)))

(with-eval-after-load 'al-appt
  (when (require 'al-file nil t)
    (al/setq-file
     al/appt-notify-normal-sound (al/sound-dir-file "drums.wav")
     al/appt-notify-urgent-sound (al/sound-dir-file "bell.oga"))))

(with-eval-after-load 'al-notification
  (setq al/timer-format "%Mm %Ss")
  (when (require 'al-file nil t)
    (al/setq-file
     al/notification-sound (al/sound-dir-file "alarm.wav"))))

(al/bind-keys
 ("C-c s" . al/sauron-toggle-hide-show)
 ("C-c S" . al/sauron-restart))

(with-eval-after-load 'sauron
  (setq
   sauron-max-line-length 174
   sauron-separate-frame nil
   sauron-modules nil
   sauron-nick-insensitivity 10
   sauron-scroll-to-bottom nil))

;;; time.el ends here
