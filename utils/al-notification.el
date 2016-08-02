;;; al-notification.el --- Additional functionality for various notifications

;; Copyright © 2014-2016 Alex Kost

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

;;; Code:

(require 'timer)
(require 'notifications)
(require 'al-file)


;;; Playing sounds

(defvar al/notification-sound
  (al/file-if-exists "/usr/share/sounds/freedesktop/stereo/bell.oga")
  "Default notification sound used by `al/timer-set'.")

(defvar al/sound-program (executable-find "play")
  "Default program for playing a sound.
Used in `al/play-sound'.
If nil, use `play-sound-file'.")

(defvar al/sound-args (and al/sound-program '("-q"))
  "List of default arguments for `al/sound-program'.")

(declare-function al/start-process "al/process" (program &rest args))

;;;###autoload
(defun al/play-sound (file)
  "Play audio FILE with `al/sound-program'."
  (if al/sound-program
      (progn
        (require 'al-process)
        (apply #'al/start-process
               al/sound-program
               (append al/sound-args (list file))))
    (play-sound-file file)))


;;; Timers

(defvar al/timer nil
  "Current timer.")

(defvar al/timer-format "%M:%S"
  "Format string for the time message.")

;;;###autoload
(defun al/timer-set (msg seconds)
  "Notify with a sound and a message MSG in some SECONDS.
Interactively, prompt for the message and the number of minutes.
With prefix, prompt for the number of seconds."
  (interactive
   (list (read-string "Message: " nil nil "You should do something!")
         (if current-prefix-arg
             (read-number "Seconds for the timer: ")
           (* 60 (read-number "Minutes for the timer: ")))))
  (al/timer-cancel)
  (setq al/timer
        (run-at-time seconds nil
                     (lambda (msg)
                       (when al/notification-sound
                         (al/play-sound al/notification-sound))
                       (notifications-notify :title "Timer" :body msg))
                     msg))
  (message "The timer has been set on %s."
           (format-time-string "%T" (timer--time al/timer))))

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
