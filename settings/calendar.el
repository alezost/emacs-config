;;; calendar.el --- Settings for `calendar' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros)
  (require 'al-aux-macros))

(require 'calendar)
(require 'diary-lib)
(require 'al-places)
(require 'al-general)

(defvar al/lazy-moving-map)

(al/bind-keys
  :map calendar-mode-map
  :parent al/lazy-moving-map
  ("t"   'calendar-goto-today)
  ("z"   'calendar-unmark)
  ("l"   'holidays)
  ([remap backward-char]  'calendar-backward-day)
  ([remap forward-char]   'calendar-forward-day)
  ([remap previous-line]  'calendar-backward-week)
  ([remap next-line]      'calendar-forward-week)
  ("C-⇤" 'calendar-beginning-of-week)
  ("C-⇥" 'calendar-end-of-week)
  ("M-←" 'calendar-backward-month)
  ("M-→" 'calendar-forward-month)
  ("M-↑" 'calendar-scroll-calendar-right)
  ("M-↓" 'calendar-scroll-calendar-left)
  ("M-S-⇤" 'calendar-beginning-of-month)
  ("M-S-⇥" 'calendar-end-of-month)
  ("H-↑" 'calendar-backward-year)
  ("H-↓" 'calendar-forward-year)
  ("n"   'al/diary-insert-entry)
  ("i d" 'al/diary-insert-entry))

(setq
 diary-file (al/notes-dir-file "diary")
 calendar-week-start-day 1
 calendar-date-display-form '(dayname ", " day " " monthname " " year)
 calendar-mark-diary-entries-flag t
 ;; Do not ruin the mode-line.
 calendar-mode-line-format nil

 diary-number-of-entries 3
 diary-comment-start "#")

(al/call-at-hook calendar-mode-hook al/bar-cursor-type)
(add-hook 'calendar-today-visible-hook #'calendar-mark-today)
(add-hook 'diary-list-entries-hook #'diary-sort-entries t)

;;; calendar.el ends here
