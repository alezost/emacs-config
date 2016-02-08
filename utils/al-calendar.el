;;; al-calendar.el --- Additional functionality for calendar and diary

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 1 Jun 2014

;;; Code:

(require 'calendar)

(defvar al/calendar-date-display-form calendar-date-display-form
  "Variable used in `al/diary-insert-entry'.")

;;;###autoload
(defun al/diary-insert-entry (arg &optional event)
  "Replacement for `diary-insert-entry'.
Use `al/calendar-date-display-form' for inserted entry instead
of `calendar-date-display-form'."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let ((calendar-date-display-form al/calendar-date-display-form))
    (diary-insert-entry arg event)))

;;;###autoload
(defun al/diary-file ()
  "Visit `diary-file'."
  (interactive)
  (find-file diary-file))

(provide 'al-calendar)

;;; al-calendar.el ends here
