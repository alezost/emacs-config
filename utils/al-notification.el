;;; al-notification.el --- Additional functionality for various notifications

;; Copyright © 2014–2016, 2021 Alex Kost

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

(require 'cl-lib)
(require 'timer)
(require 'notifications)
(require 'al-file)

(defvar al/notification-sound
  (al/file-if-exists "/usr/share/sounds/freedesktop/stereo/bell.oga")
  "Default notification sound used by `al/timer-set'.")


;;; Timers

(defvar al/timer nil
  "Current timer.")

(defvar al/timer-format "%M:%S"
  "Format string for the time message.")

(declare-function al/play-sound "al-sound" (file))

;;;###autoload
(defun al/timer-set (seconds &optional msg)
  "Notify in some SECONDS with a sound and a message MSG.
Interactively, prompt for the number of minutes.
With \\[universal-argument], prompt for the message as well.
With \\[universal-argument] \\[universal-argument], prompt for
the number of seconds and the message.
If the prefix argument is numerical, use it as the number of minutes."
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
        (run-at-time seconds nil #'al/timer-notify (or msg "Break!")))
  (message "The timer has been set on %s."
           (format-time-string "%T" (timer--time al/timer))))

(defun al/timer-notify (message)
  "Notify with MESSAGE."
  (when (and al/notification-sound
             (require 'al-sound nil t))
    (al/play-sound al/notification-sound))
  (notifications-notify :title "Timer" :body message))

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
  (al/timer-funcall-on-active-timer
   (lambda (sec)
     (cancel-timer al/timer)
     (setq al/timer nil)
     (message "The timer has been cancelled."))))

(provide 'al-notification)

;;; al-notification.el ends here
