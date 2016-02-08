;;; utl-pcomplete.el --- Additional functionality for pcomplete

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 9 Jun 2015

;;; Code:

(defun utl-pcomplete-no-space ()
  "Do not terminate a completion with space in the current buffer."
  (setq-local pcomplete-termination-string ""))

(provide 'utl-pcomplete)

;;; utl-pcomplete.el ends here
