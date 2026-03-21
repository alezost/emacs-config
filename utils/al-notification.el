;;; al-notification.el --- Additonal functionality for timers and notifications  -*- lexical-binding: t -*-

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

(require 'seq)
(require 'timer)
(require 'notifications)
(require 'al-misc)
(require 'al-file)

(defvar al/notification-sound
  (al/file-if-exists "/usr/share/sounds/freedesktop/stereo/bell.oga")
  "Default notification sound used by `al/notification'.")

(declare-function al/play-sound "al-sound" (file))

(defun al/notification-notify (&rest args)
  "Play `al/notification-sound' and send notification.
Pass ARGS to `notifications-notify'."
  (when (and al/notification-sound
             (require 'al-sound nil t))
    (al/play-sound al/notification-sound))
  (apply #'notifications-notify args))


;;; Interface for notifications

(defvar al/notifications nil
  "Property list of active notifications.
Each KEYWORD of this plist should be either `:timer' (where the
currently active timer is stored) or any keyword supported by
`notifications-notify'.")

(defvar al/notification-time-format "%M:%S"
  "Format string for notification time.")

(defvar al/timer-mode)  ; defined below (needed to scilence compiler)

(defun al/notification-new (seconds &rest args)
  "Create a new notification timer and add it to `al/notifications'
SECONDS is the number of seconds for the created timer.
ARGS are any arguments supported by `notifications-notify'."
  (let ((timer (run-at-time seconds nil
                            #'apply #'al/notification-notify args)))
    (push (append (list :timer timer) args)
          al/notifications)
    (unless al/timer-mode
      (al/timer-mode 1))
    (message "A new notification has been set on %s."
             (format-time-string "%T" (timer--time timer)))))

(defun al/notification-cleanup ()
  "Remove expired notifications from `al/notifications'."
  (setq al/notifications
        (seq-filter (lambda (notif)
                      (al/timer-live? (plist-get notif :timer)))
                    al/notifications)))

(defun al/notification-kill-timer (timer)
  "Cancel TIMER and remove its notification from `al/notifications'."
  (interactive)
  (setq al/notifications
        (seq-keep
         (lambda (notif)
           (if (not (eq timer (plist-get notif :timer)))
               notif
             (cancel-timer timer)
             nil))
         al/notifications)))

(defun al/notification-kill-all-timers ()
  "Cancel all active timers and clear `al/notifications'."
  (interactive)
  (dolist (notif al/notifications)
    (cancel-timer (plist-get notif :timer)))
  (setq al/notifications nil)
  (al/timer-mode -1))


;;; General functionality for timers

(defun al/timer-remaining-seconds (timer)
  "Return the number of seconds left until the deadline of TIMER.
The result is negative, if TIMER is elapsed.
Return nil if TIMER is not a proper timer."
  (and (timerp timer)
       (- (timer-until timer (current-time)))))

(defun al/timer-live? (timer)
  "Return t if TIMER is not expired."
  (let ((seconds (al/timer-remaining-seconds timer)))
    (and seconds (< 0 seconds))))


;;; Timers in the mode line

(defvar al/timer-mode-line-update-time 3
  "Time (in seconds) to update the mode line.")

(defvar al/timer-mode-line-timer nil)

(defvar al/timer-mode-line-string "")
;; (put 'al/timer-mode-line-string 'risky-local-variable t)

(defun al/timer-update-mode-line ()
  (let ((times
         (seq-keep
          (lambda (notif)
            (let* ((timer (plist-get notif :timer))
                   (seconds (al/timer-remaining-seconds timer)))
              (when (< 0 seconds)
                (format-time-string al/notification-time-format
                                    (seconds-to-time seconds)))))
          al/notifications)))
    (if times
        (setq al/timer-mode-line-string
              (concat " 🕒 " (mapconcat #'identity times ", ")))
      (al/timer-mode -1))
    (force-mode-line-update)))

(define-minor-mode al/timer-mode
  "Toggle displaying active timers in the mode line."
  :global t
  :group 'al/timer
  (when al/timer-mode-line-timer
    (cancel-timer al/timer-mode-line-timer))
  (setq al/timer-mode-line-string "")
  (if al/timer-mode
      ;; Turn on.
      (progn
        (if global-mode-string
            (al/add-to-list-after 'global-mode-string ""
                                  'al/timer-mode-line-string)
          (setq global-mode-string '("" al/timer-mode-line-string)))
        (setq al/timer-mode-line-timer
              (run-with-timer 0 al/timer-mode-line-update-time
                              #'al/timer-update-mode-line)))
    ;; Turn off.
    (setq global-mode-string
          (remove 'al/timer-mode-line-string
                  global-mode-string)))
  (force-mode-line-update))

(provide 'al-notification)

;;; al-notification.el ends here
