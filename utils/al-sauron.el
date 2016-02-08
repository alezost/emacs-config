;;; al-sauron.el --- Additional functionality for sauron

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 21 May 2014

;;; Code:

(require 'sauron)

;;;###autoload
(defun al/sauron-toggle-hide-show ()
  "Toggle between showing/hiding the sauron window or frame.
If sauron is shown, switch to its window."
  (interactive)
  (sauron-toggle-hide-show)
  (let ((buf (get-buffer-window sr-buffer)))
    (and buf (select-window buf))))

;;;###autoload
(defun al/sauron-restart ()
  "Restart sauron."
  (interactive)
  (sauron-stop)
  (sauron-start))

(provide 'al-sauron)

;;; al-sauron.el ends here
