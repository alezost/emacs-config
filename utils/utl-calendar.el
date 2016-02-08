;;; utl-calendar.el --- Additional functionality for calendar and diary

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 1 Jun 2014

;;; Code:

(require 'calendar)

(defvar utl-calendar-date-display-form calendar-date-display-form
  "Variable used in `utl-diary-insert-entry'.")

;;;###autoload
(defun utl-diary-insert-entry (arg &optional event)
  "Replacement for `diary-insert-entry'.
Use `utl-calendar-date-display-form' for inserted entry instead
of `calendar-date-display-form'."
  (interactive
   (list current-prefix-arg last-nonmenu-event))
  (let ((calendar-date-display-form utl-calendar-date-display-form))
    (diary-insert-entry arg event)))

;;;###autoload
(defun utl-diary-file ()
  "Visit `diary-file'."
  (interactive)
  (find-file diary-file))

(provide 'utl-calendar)

;;; utl-calendar.el ends here
