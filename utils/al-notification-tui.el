;;; al-notification-tui.el --- Transient interface for timers and notifications  -*- lexical-binding: t -*-

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
(require 'transient)
(require 'al-notification)
(require 'al-color)


;;; Transient interface for notifications

(defvar al/notification-time 45
  "Default time (in minutes) for a new timer.")

(defvar al/notification-quick-time 4
  "Default time (in minutes) for a new quick timer.")

(defvar al/notification-timeout 0
  "Default timeout (in seconds) for notification message.")

(defun al/notification-kill-keys ()
  "Return a list of transient keys to kill active timers."
  (seq-map-indexed
   (lambda (notif index)
     (let* ((index    (1+ index))
            (msg      (plist-get notif :body))
            (timer    (plist-get notif :timer))
            (seconds  (al/timer-remaining-seconds timer))
            (time-str (format-time-string
                       al/notification-time-format
                       (seconds-to-time (abs seconds))))
            (time-str (al/with-face 'font-lock-constant-face time-str))
            (msg      (al/with-face 'font-lock-string-face   msg)))
       (list (concat "k" (number-to-string index))
             (concat "kill timer [" msg ", "
                     (if (< 0 seconds)
                         (concat time-str " left")
                       (concat "expired " time-str " ago"))
                     "]")
             (lambda ()
               (interactive)
               (al/notification-kill-timer timer)
               (al/notification)))))
   al/notifications))

(transient-define-suffix al/notification:kill-all ()
  "Cancel all active timers and clear `al/notifications'."
  (interactive)
  (dolist (notif al/notifications)
    (cancel-timer (plist-get notif :timer)))
  (setq al/notifications nil)
  (al/timer-mode -1)
  (al/notification))

(transient-define-suffix al/notification:list-timers ()
  "Call `list-timers'."
  :description "list all timers (including system ones)"
  :key "l"
  (interactive)
  (list-timers))

(defun al/notification-args ()
  "Return list of arguments for the current `al/notification' transient.
The first argument in this list is the number of seconds and the rest
arguments is a plist suitable for `notifications-notify'."
  (let* ((args    (transient-args 'al/notification))
         (time    (transient-arg-value "time=" args))
         (msg     (transient-arg-value "message=" args))
         (title   (transient-arg-value "title=" args))
         (timeout (transient-arg-value "timeout=" args))
         (seconds (* 60 (string-to-number time)))
         ;; `:timeout' must be in milliseconds for `notifications-notify'.
         (timeout (and timeout (* 1000 (string-to-number timeout)))))
    (list seconds :body msg :title title :timeout timeout)))

(transient-define-suffix al/notification:new (seconds &rest args)
  "Send notification in SECONDS.
Pass ARGS to `notifications-notify'."
  (interactive (al/notification-args))
  (let ((timer (run-at-time seconds nil
                            #'apply #'al/notification-notify args)))
    (push (append (list :timer timer) args)
          al/notifications)
    (unless al/timer-mode
      (al/timer-mode 1))
    (message "A new notification has been set on %s."
             (format-time-string "%T" (timer--time timer)))))

(defun al/notification-quick-string ()
  (format "set %d min timer" al/notification-quick-time))

(transient-define-suffix al/notification:new-quick (_ &rest args)
  "Send notification in `al/notification-quick-time' minutes.
Pass ARGS to `notifications-notify'."
  :description #'al/notification-quick-string
  :key "M-T"
  (interactive (al/notification-args))
  (apply #'al/notification:new
         (* 60 al/notification-quick-time)
         args))

(transient-define-argument al/notification:title ()
  :description "title"
  :class 'transient-option
  :key "-T"
  :always-read t
  :prompt "Notification title: "
  :argument "title=")

(transient-define-argument al/notification:message ()
  :description "message"
  :class 'transient-option
  :key "m"
  :always-read t
  :prompt "Notification message: "
  :argument "message=")

(transient-define-argument al/notification:timeout ()
  :description "timeout (seconds)"
  :class 'transient-option
  :key "-t"
  :prompt "Notification timeout (seconds): "
  :reader 'al/notification-read-seconds
  :argument "timeout=")

(defun al/notification-read-number (prompt initial-input history)
  (number-to-string (read-number prompt initial-input history)))

(transient-define-argument al/notification:time ()
  :description "time (minutes)"
  :class 'transient-option
  :key "t"
  :always-read t
  :prompt "Time (minutes): "
  :reader 'al/notification-read-number
  :argument "time=")

(defun al/notification:default-value ()
  (list "title=Timer"
        "message=Break!"
        (format "time=%d" al/notification-time)
        (format "timeout=%d" al/notification-timeout)))

;;;###autoload (autoload 'al/notification "al-notification" nil t)
(transient-define-prefix al/notification ()
  "Interface to set and kill timers."
  :value 'al/notification:default-value
  'al/notification:kill-group
  ["Notification parameters"
   [(al/notification:title)
    (al/notification:timeout)
    ""
    (al/notification:time)]
   [(al/notification:message)]]
  ["New notification"
   [("n" "set new timer" al/notification:new)]
   [(al/notification:new-quick)]]
  (interactive)
  (al/notification-cleanup)
  (if al/notifications
      (eval
       `(transient-define-group al/notification:kill-group
          [ ;; :if-non-nil al/notifications
           :pad-keys t
           "Active timers"
           ,@(al/notification-kill-keys)
           ("K" "kill all timers" al/notification:kill-all)
           (al/notification:list-timers)]))
    (transient-define-group al/notification:kill-group
      ["Active timers"
       (al/notification:list-timers)]))
  (transient-setup 'al/notification))

(provide 'al-notification-tui)

;;; al-notification-tui.el ends here
