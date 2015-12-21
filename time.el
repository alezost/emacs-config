;;; time.el --- Time, calendar, diary, appointments, notifications, …

;; Copyright © 2014-2015 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Global keys

(al/bind-keys
 :prefix-map al/calendar-map
 :prefix-docstring "Map for calendar, diary, notifications, etc."
 :prefix "M-C"
 ("M-C" . calendar)
 ("c"   . calendar)
 ("d"   . diary)
 ("D"   . utl-diary-file)
 ("A"   . appt-activate)
 ("a n" . appt-add)
 ("a k" . appt-delete)
 ("M-T"   (utl-timer-set "Tea!!" 180))
 ("T"     (utl-timer-set "Break!!" (* 45 60)))
 ("t n" . utl-timer-set)
 ("t t" . utl-timer-remaining-time)
 ("t k" . utl-timer-cancel))


;;; Misc settings and packages

(use-package time
  :defer t
  :config
  (setq
   display-time-interval 5
   display-time-format " %H:%M:%S"))

(use-package calendar
  :defer t
  :init
  (setq calendar-date-style 'iso)
  :config
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
   ("C-п" . calendar-end-of-week)
   ("M-o" . calendar-backward-month)
   ("M-u" . calendar-forward-month)
   ("M-." . calendar-scroll-right-three-months)
   ("M-e" . calendar-scroll-left-three-months)
   ("M-A" . calendar-beginning-of-month)
   ("M-I" . calendar-end-of-month)
   ("H-." . calendar-backward-year)
   ("H-e" . calendar-forward-year)
   ("n"   . utl-diary-insert-entry)
   ("i d" . utl-diary-insert-entry))

  ;; Do not ruin the mode-line.
  (if (version< emacs-version "25")
      (advice-add 'calendar-update-mode-line :override 'ignore)
    (setq calendar-mode-line-format nil))

  (al/add-hook-maybe 'calendar-mode-hook 'al/bar-cursor-type)
  (add-hook 'calendar-today-visible-hook 'calendar-mark-today))

(use-package utl-calendar
  :defer t
  :config
  (setq utl-calendar-date-display-form
        '((format "%s %.3s %2s" year monthname day))))

(use-package solar
  :defer t
  :config
  (setq
   calendar-latitude 50.6
   calendar-longitude 36.6
   calendar-location-name "home"
   calendar-time-display-form
   '(24-hours ":" minutes
     (if time-zone " (") time-zone (if time-zone ")"))))

(use-package diary-lib
  :defer t
  :config
  (setq
   diary-number-of-entries 3
   diary-comment-start "#")
  (add-hook 'diary-list-entries-hook 'diary-sort-entries t))

(use-package appt
  :defer 5
  :if (utl-server-running-p)
  :config
  (setq
   appt-audible nil
   appt-display-diary nil
   appt-message-warning-time 5
   appt-display-interval 1)
  (when (require 'sauron nil t)
    (add-to-list 'sauron-modules 'sauron-org))
  (when (require 'utl-appt nil t)
    (advice-add 'appt-display-message
      :override 'utl-appt-display-message))
  (appt-activate))

(use-package utl-appt
  :defer t
  :config
  (setq
   utl-appt-notify-normal-sound (al/sound-dir-file "drums.wav")
   utl-appt-notify-urgent-sound (al/sound-dir-file "bell.oga")))

(use-package utl-notification
  :defer t
  :commands utl-play-sound
  :config
  (setq
   utl-sound-file (al/sound-dir-file "alarm.wav")
   utl-timer-format "%M min %S sec"))

(use-package notifications
  :defer t
  :config
  ;; XXX Remove when dunst will support icons.
  (setq notifications-application-icon ""))

(use-package sauron
  :defer t
  :init
  (al/bind-keys
   ("C-c s" . utl-sauron-toggle-hide-show)
   ("C-c S" . utl-sauron-restart))
  :config
  (setq
   sauron-max-line-length 174
   sauron-separate-frame nil
   sauron-modules nil
   sauron-nick-insensitivity 10
   sauron-scroll-to-bottom nil))

;;; time.el ends here
