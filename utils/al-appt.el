;;; al-appt.el --- Additional functionality for appointments

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 17 Dec 2014

;;; Code:

(require 'al-notification)

(defvar al/appt-actions
  '((5 . al/appt-notify-normal)
    (0 . al/appt-notify-urgent))
  "Alist of minutes left for an appointment and action functions.
Each function is called with an appointment string when the
according number of minutes is left.")

(defvar al/appt-notify-normal-sound
  "/usr/share/sounds/freedesktop/stereo/bell.oga"
  "Audio file used by `al/appt-notify-normal'.")

(defvar al/appt-notify-urgent-sound
  "/usr/share/sounds/freedesktop/stereo/complete.oga"
  "Audio file used by `al/appt-notify-urgent'.")

(defun al/appt-notify-sound (sound)
  "Notify about an appointment by playing SOUND."
  (al/play-sound sound))

(defun al/appt-notify-sound-message (sound string)
  "Notify about an appointment by playing SOUND and displaying STRING."
  (al/play-sound sound)
  (notifications-notify :title "Appointment"
                        :body string))

(defun al/appt-notify-normal (&optional _)
  "Notify about an appointment in a normal way.
Use `al/appt-notify-normal-sound'."
  (al/appt-notify-sound al/appt-notify-normal-sound))

(defun al/appt-notify-urgent (string)
  "Notify about an appointment in an urgent way.
Use `al/appt-notify-urgent-sound'."
  (al/appt-notify-sound-message al/appt-notify-urgent-sound
                                 string))

(defun al/appt-display-message (strings mins)
  "Notify about an appointment if needed.
This function is a substitution for `appt-display-message',
because I know better what to do."
  (let* ((string (car strings))
         (min    (car mins))
         (fun    (cdr (assq min al/appt-actions))))
    (and fun (funcall fun string))))

(provide 'al-appt)

;;; al-appt.el ends here
