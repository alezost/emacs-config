;;; al-notification.el --- Additional functionality for various notifications

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 6 Jun 2014

;;; Code:

(require 'timer)
(require 'notifications)


;;; Playing sounds

(defvar utl-sound-file nil
  "Default sound file for `utl-play-sound'.")

(defvar utl-sound-program (executable-find "play")
  "Default program for playing a sound.
Used in `utl-play-sound'.
If nil, use `play-sound-file'.")

(defvar utl-sound-args (and utl-sound-program '("-q"))
  "List of default arguments for `utl-sound-program'.")

(declare-function utl-start-process "utl-process" (program &rest args))

;;;###autoload
(defun utl-play-sound (&optional file)
  "Play audio FILE with `utl-sound-program'.
If FILE is nil, use `utl-sound-file'."
  (or file (setq file utl-sound-file))
  (if utl-sound-program
      (progn
        (require 'utl-process)
        (apply #'utl-start-process
               utl-sound-program
               (append utl-sound-args (list file))))
    (play-sound-file file)))


;;; Timers

(defvar utl-timer nil
  "Current timer.")

(defvar utl-timer-format "%M:%S"
  "Format string for the time message.")

;;;###autoload
(defun utl-timer-set (msg seconds)
  "Notify with a sound and a message MSG in some SECONDS.
Interactively, prompt for the message and the number of minutes.
With prefix, prompt for the number of seconds."
  (interactive
   (list (read-string "Message: " nil nil "You should do something!")
         (if current-prefix-arg
             (read-number "Seconds for the timer: ")
           (* 60 (read-number "Minutes for the timer: ")))))
  (utl-timer-cancel)
  (setq utl-timer
        (run-at-time seconds nil
                     (lambda (msg)
                       (utl-play-sound)
                       (notifications-notify :title "Timer" :body msg))
                     msg))
  (message "The timer has been set on %s."
           (format-time-string "%T" (timer--time utl-timer))))

(defun utl-timer-funcall-on-active-timer (fun &optional silent)
  "Call function FUN if current timer is active.

If timer is not active, display a message about it, unless SILENT
is non-nil.

FUN is called with a single argument - the number of seconds left
for the current timer."
  (let ((seconds (utl-timer-remaining-seconds)))
    (if (or (null seconds) (< seconds 0))
        (or silent (message "No active timer."))
      (funcall fun seconds))))

(defun utl-timer-remaining-seconds ()
  "Return the number of seconds left until the deadline of `utl-timer'.
The result is negative, if the timer is elapsed.
Return nil if `utl-timer' is not a proper timer."
  (and (timerp utl-timer)
       (- (timer-until utl-timer (current-time)))))

(defun utl-timer-remaining-time ()
  "Show the time left until the deadline of `utl-timer'."
  (interactive)
  (utl-timer-funcall-on-active-timer
   (lambda (sec)
     (message "Time left: %s."
              (format-time-string utl-timer-format
                                  (seconds-to-time sec))))))

(defun utl-timer-cancel ()
  "Cancel current timer."
  (interactive)
  (utl-timer-funcall-on-active-timer
   (lambda (sec)
     (cancel-timer utl-timer)
     (setq utl-timer nil)
     (message "The timer has been cancelled."))))

(provide 'al-notification)

;;; al-notification.el ends here
