;;; utl-url.el --- Code for searching and downloading various stuff

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 26 Apr 2015

;;; Code:

(require 'wget)

(defvar utl-url-mp3-re
  (rx "http" (? ?s) "://" (1+ any) ".mp3")
  "Regexp for mp3 file.")

;;;###autoload
(defun utl-url-wget-mp3 (url)
  "Download the first mp3 file from URL with `wget'."
  (interactive
   (list (read-string "Download mp3 from URL: "
                      (thing-at-point 'url))))
  (let* ((buf (url-retrieve-synchronously url))
         (mp3 (with-current-buffer buf
                (re-search-forward utl-url-mp3-re)
                (match-string 0))))
    (when (y-or-n-p (format "Download '%s'? " mp3))
      (wget mp3))))

(provide 'utl-url)

;;; utl-url.el ends here
