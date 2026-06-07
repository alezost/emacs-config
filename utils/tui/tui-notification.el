;;; tui-notification.el --- Transient interface for timers and notifications  -*- lexical-binding: t -*-

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
(require 'transient)
(require 'al-notification)
(require 'al-visual)


;;; Transient interface for notifications

(defvar tui/notification-time 45
  "Default time (in minutes) for a new timer.")

(defvar tui/notification-quick-time 4
  "Default time (in minutes) for a new quick timer.")

(defvar tui/notification-timeout 0
  "Default timeout (in seconds) for notification message.")

(defun tui/notification-kill-keys ()
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
               (tui/notification)))))
   al/notifications))

(transient-define-suffix tui/notification:kill-all ()
  "Cancel all active timers and clear `al/notifications'."
  (interactive)
  (al/notification-kill-all-timers)
  (tui/notification))

(transient-define-suffix tui/notification:list-timers ()
  "Call `list-timers'."
  :description "list all timers (including system ones)"
  :key "l"
  (interactive)
  (list-timers))

(defun tui/notification-args ()
  "Return list of arguments for the current `tui/notification' transient.
The first argument in this list is the number of seconds and the rest
arguments is a plist suitable for `notifications-notify'."
  (let* ((args    (transient-args 'tui/notification))
         (time    (transient-arg-value "time=" args))
         (msg     (transient-arg-value "message=" args))
         (title   (transient-arg-value "title=" args))
         (timeout (transient-arg-value "timeout=" args))
         (seconds (* 60 (string-to-number time)))
         ;; `:timeout' must be in milliseconds for `notifications-notify'.
         (timeout (and timeout (* 1000 (string-to-number timeout)))))
    (list seconds :body msg :title title :timeout timeout)))

(transient-define-suffix tui/notification:new (seconds &rest args)
  "Send notification in SECONDS.
Pass ARGS to `notifications-notify'."
  (interactive (tui/notification-args))
  (apply #'al/notification-new seconds args))

(defun tui/notification-quick-string ()
  (format "set %d min timer" tui/notification-quick-time))

(transient-define-suffix tui/notification:new-quick (_ &rest args)
  "Send notification in `tui/notification-quick-time' minutes.
Pass ARGS to `notifications-notify'."
  :description #'tui/notification-quick-string
  :key "M-T"
  (interactive (tui/notification-args))
  (apply #'tui/notification:new
         (* 60 tui/notification-quick-time)
         args))

(transient-define-argument tui/notification:title ()
  :description "title"
  :class 'transient-option
  :key "-T"
  :always-read t
  :prompt "Notification title: "
  :argument "title=")

(transient-define-argument tui/notification:message ()
  :description "message"
  :class 'transient-option
  :key "m"
  :always-read t
  :prompt "Notification message: "
  :argument "message=")

(defun tui/notification-read-number (prompt initial-input history)
  (number-to-string (read-number prompt initial-input history)))

(transient-define-argument tui/notification:timeout ()
  :description "timeout (seconds)"
  :class 'transient-option
  :key "-t"
  :prompt "Notification timeout (seconds): "
  :reader 'tui/notification-read-number
  :argument "timeout=")

(transient-define-argument tui/notification:time ()
  :description "time (minutes)"
  :class 'transient-option
  :key "t"
  :always-read t
  :prompt "Time (minutes): "
  :reader 'tui/notification-read-number
  :argument "time=")

(defun tui/notification-default-value ()
  (list "title=Timer"
        "message=Break!"
        (format "time=%d" tui/notification-time)
        (format "timeout=%d" tui/notification-timeout)))

;;;###autoload (autoload 'tui/notification "tui-notification" nil t)
(transient-define-prefix tui/notification ()
  "Interface to set and kill timers."
  :value 'tui/notification-default-value
  'tui/notification:kill-group
  ["Notification parameters"
   [(tui/notification:title)
    (tui/notification:timeout)
    ""
    (tui/notification:time)]
   [(tui/notification:message)]]
  ["New notification"
   [("n" "set new timer" tui/notification:new)]
   [(tui/notification:new-quick)]]
  (interactive)
  (al/notification-cleanup)
  (if al/notifications
      (eval
       `(transient-define-group tui/notification:kill-group
          [ ;; :if-non-nil al/notifications
           :pad-keys t
           "Active timers"
           ,@(tui/notification-kill-keys)
           ("K" "kill all timers" tui/notification:kill-all)
           (tui/notification:list-timers)]))
    (transient-define-group tui/notification:kill-group
      ["Active timers"
       (tui/notification:list-timers)]))
  (transient-setup 'tui/notification))

(provide 'tui-notification)

;;; tui-notification.el ends here
