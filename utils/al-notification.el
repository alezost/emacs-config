;;; al-notification.el --- Additional functionality for various notifications  -*- lexical-binding: t -*-

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

;;; Code:

(require 'timer)
(require 'notifications)
(require 'al-misc)
(require 'al-file)

(defvar al/notification-sound
  (al/file-if-exists "/usr/share/sounds/freedesktop/stereo/bell.oga")
  "Default notification sound used by `al/timer-set'.")


;;; Timers

(defvar al/timer nil
  "Current timer.")

(defvar al/timer-format "%M:%S"
  "Format string for the time message.")

(defvar al/timer-timeout 0
  "Default timeout for timer notification message.")

(declare-function al/play-sound "al-sound" (file))

;;;###autoload
(defun al/timer-set (seconds &optional msg &rest args)
  "Notify in some SECONDS with a sound and a message MSG.
Interactively, prompt for the number of minutes.
With \\[universal-argument], prompt for the message as well.
With \\[universal-argument] \\[universal-argument], prompt for
the number of seconds and the message.
If the prefix argument is numerical, use it as the number of minutes.
Pass the rest ARGS to `al/timer-notify'."
  (interactive
   (list
    (cond ((numberp current-prefix-arg)
           (* 60 current-prefix-arg))
          ((equal current-prefix-arg '(16))
           (read-number "Seconds for the timer: "))
          (t (* 60 (read-number "Minutes for the timer: "))))
    (and (consp current-prefix-arg)
         (read-string "Message: " nil nil "You should do something!"))))
  (al/timer-cancel)
  (setq al/timer
        (run-at-time seconds nil
                     #'apply #'al/timer-notify (or msg "Break!") args))
  (al/timer-mode 1)
  (message "The timer has been set on %s."
           (format-time-string "%T" (timer--time al/timer))))

(defun al/timer-notify (message &rest args)
  "Notify with MESSAGE.
Pass the rest ARGS to `notifications-notify'."
  (when (and al/notification-sound
             (require 'al-sound nil t))
    (al/play-sound al/notification-sound))
  (let* ((args (if (plist-get args :title)
                   args
                 (plist-put args :title "Timer")))
         (args (if (plist-get args :timeout)
                   args
                 (plist-put args :timeout al/timer-timeout))))
    (apply #'notifications-notify :body message args)
    (al/timer-mode -1)))

(defun al/timer-funcall-on-active-timer (fun &optional silent)
  "Call function FUN if current timer is active.

If timer is not active, display a message about it, unless SILENT
is non-nil.

FUN is called with a single argument - the number of seconds left
for the current timer."
  (let ((seconds (al/timer-remaining-seconds)))
    (if (or (null seconds) (< seconds 0))
        (or silent (message "No active timer."))
      (funcall fun seconds))))

(defun al/timer-remaining-seconds ()
  "Return the number of seconds left until the deadline of `al/timer'.
The result is negative, if the timer is elapsed.
Return nil if `al/timer' is not a proper timer."
  (and (timerp al/timer)
       (- (timer-until al/timer (current-time)))))

(defun al/timer-remaining-time ()
  "Show the time left until the deadline of `al/timer'."
  (interactive)
  (al/timer-funcall-on-active-timer
   (lambda (sec)
     (message "Time left: %s."
              (format-time-string al/timer-format
                                  (seconds-to-time sec))))))

(defun al/timer-cancel ()
  "Cancel current timer."
  (interactive)
  (al/timer-mode -1)
  (al/timer-funcall-on-active-timer
   (lambda (_sec)
     (cancel-timer al/timer)
     (setq al/timer nil)
     (message "The timer has been cancelled."))))


;;; Timer in the mode line

(defvar al/timer-mode-line-update-time 3
  "Time (in seconds) to update the mode line.")

(defvar al/timer-mode-line-timer nil)

(defvar al/timer-mode-line-string "")
;; (put 'al/timer-mode-line-string 'risky-local-variable t)

(defun al/timer-update-mode-line ()
  (al/timer-funcall-on-active-timer
   (lambda (sec)
     (setq al/timer-mode-line-string
           (concat " 🕒 "
                   (format-time-string al/timer-format
                                       (seconds-to-time sec)))))
   (force-mode-line-update)))

(define-minor-mode al/timer-mode
  "Toggle displaying timer in the mode line."
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
