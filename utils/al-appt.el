;;; al-appt.el --- Additional functionality for appointments  -*- lexical-binding: t -*-

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

(require 'notifications)
(require 'al-sound)
(require 'al-file)

(defvar al/appt-actions
  '((5 . al/appt-notify-normal)
    (0 . al/appt-notify-urgent))
  "Alist of minutes left for an appointment and action functions.
Each function is called with an appointment string when the
according number of minutes is left.")

(defvar al/appt-notify-normal-sound
  (al/file-if-exists
   "/usr/share/sounds/freedesktop/stereo/bell.oga")
  "Audio file used by `al/appt-notify-normal'.")

(defvar al/appt-notify-urgent-sound
  (al/file-if-exists
   "/usr/share/sounds/freedesktop/stereo/complete.oga")
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

(defun al/appt-mode-line (min-list &rest _)
  "Replacement for `appt-mode-line'."
  (format " appt%s in %s"
          (if (cdr min-list) "s" "")
          (mapconcat #'identity min-list ",")))

;; `appt-check' adds an extra space to the mode line which can be fixed
;; by the following code, and there is no other way around.  However, I
;; don't like to call `force-mode-line-update' 2 times in a row (in
;; `appt-check' and in `al/appt-fix-mode-string').  So I made a hard
;; decision to bear with this ugly extra space.

;; (defun al/appt-fix-mode-string (&rest _)
;;   "Remove the last character from `appt-mode-string'.
;; This function is intended to be used as an 'after' advice for
;; `appt-check', which hardcodes the trailing space in `appt-mode-string'."
;;   (when appt-mode-string
;;     (setq appt-mode-string (substring appt-mode-string 0 -1))
;;     (force-mode-line-update t)))
;;
;; (advice-add 'appt-check :after #'al/appt-fix-mode-string)

(provide 'al-appt)

;;; al-appt.el ends here
