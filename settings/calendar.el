;;; calendar.el --- Settings for `calendar' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros)
  (require 'al-key))

(require 'calendar)
(require 'diary-lib)
(require 'solar)
(require 'al-places)
(require 'al-general)
(require 'al-calendar)

(al/bind-keys
  :map calendar-mode-map
  ("t"   'calendar-goto-today)
  ("←"   'calendar-backward-day)
  ("→"   'calendar-forward-day)
  ("↑"   'calendar-backward-week)
  ("↓"   'calendar-forward-week)
  ("z"   'calendar-unmark)
  ("l"   'holidays)
  ("C-⇤" 'calendar-beginning-of-week)
  ("C-⇥" 'calendar-end-of-week)
  ("M-←" 'calendar-backward-month)
  ("M-→" 'calendar-forward-month)
  ("M-↑" 'calendar-scroll-right-three-months)
  ("M-↓" 'calendar-scroll-left-three-months)
  ("M-S-⇤" 'calendar-beginning-of-month)
  ("M-S-⇥" 'calendar-end-of-month)
  ("H-." 'calendar-backward-year)
  ("H-e" 'calendar-forward-year)
  ("n"   'al/diary-insert-entry)
  ("i d" 'al/diary-insert-entry))

(setq
 diary-file (al/notes-dir-file "diary")
 calendar-week-start-day 1
 calendar-date-display-form '(dayname ", " day " " monthname " " year)
 calendar-mark-diary-entries-flag t
 ;; Do not ruin the mode-line.
 calendar-mode-line-format nil

 calendar-latitude 50.6
 calendar-longitude 36.6
 calendar-location-name "home"
 calendar-time-display-form
 '(24-hours ":" minutes
            (if time-zone " (") time-zone (if time-zone ")"))

 diary-number-of-entries 3
 diary-comment-start "#"

 al/calendar-date-display-form
 '((format "%s %.3s %2s" year monthname day)))

(al/call-at-hook calendar-mode-hook al/bar-cursor-type)
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'diary-list-entries-hook #'diary-sort-entries t)

;;; calendar.el ends here
