;;; al-sr.el --- Additional functionality for Sunrise Commander

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 8 Nov 2012

;;; Code:

(require 'sunrise-commander)

;;;###autoload
(defun al/sr-toggle ()
  "Toggle Sunrise Commander.
If sr is active - show dired with current directory.
If dired is active - show sr with current directory."
  (interactive)
  (let (file)
    (if sr-running
        (progn (setq sr-running nil)
               (delete-other-windows)
               (and (eq major-mode 'sr-mode)
                    (setq file (dired-get-file-for-visit))
                    (dired (dired-current-directory))
                    (dired-goto-file file)))
      (and (eq major-mode 'dired-mode)
           (setq file (dired-get-file-for-visit)))
      (sunrise)
      (and file (sr-dired file)))))

(provide 'al-sr)

;;; al-sr.el ends here
