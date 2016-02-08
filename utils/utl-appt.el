;;; utl-appt.el --- Additional functionality for appointments

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 17 Dec 2014

;;; Code:

(require 'utl-notification)

(defvar utl-appt-actions
  '((5 . utl-appt-notify-normal)
    (0 . utl-appt-notify-urgent))
  "Alist of minutes left for an appointment and action functions.
Each function is called with an appointment string when the
according number of minutes is left.")

(defvar utl-appt-notify-normal-sound
  "/usr/share/sounds/freedesktop/stereo/bell.oga"
  "Audio file used by `utl-appt-notify-normal'.")

(defvar utl-appt-notify-urgent-sound
  "/usr/share/sounds/freedesktop/stereo/complete.oga"
  "Audio file used by `utl-appt-notify-urgent'.")

(defun utl-appt-notify-sound (sound)
  "Notify about an appointment by playing SOUND."
  (utl-play-sound sound))

(defun utl-appt-notify-sound-message (sound string)
  "Notify about an appointment by playing SOUND and displaying STRING."
  (utl-play-sound sound)
  (notifications-notify :title "Appointment"
                        :body string))

(defun utl-appt-notify-normal (&optional _)
  "Notify about an appointment in a normal way.
Use `utl-appt-notify-normal-sound'."
  (utl-appt-notify-sound utl-appt-notify-normal-sound))

(defun utl-appt-notify-urgent (string)
  "Notify about an appointment in an urgent way.
Use `utl-appt-notify-urgent-sound'."
  (utl-appt-notify-sound-message utl-appt-notify-urgent-sound
                                 string))

(defun utl-appt-display-message (strings mins)
  "Notify about an appointment if needed.
This function is a substitution for `appt-display-message',
because I know better what to do."
  (let* ((string (car strings))
         (min    (car mins))
         (fun    (cdr (assq min utl-appt-actions))))
    (and fun (funcall fun string))))

(provide 'utl-appt)

;;; utl-appt.el ends here
