;;; al-org.el --- Additional functionality for org-mode

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 7 Aug 2012

;;; Code:

(require 'al-text)
(require 'org)
(require 'org-table)

(defun utl-org-get-time-stamp (time &optional with-hm)
  "Return org time stamp string from TIME (iso or system format).
WITH-HM means use the stamp format that includes the time of the day."
  (let ((fmt (funcall (if with-hm 'cdr 'car)
                      org-time-stamp-formats)))
    (and (stringp time)
         (setq time (org-read-date nil t time)))
    (format-time-string fmt time)))

(defun utl-org-get-time-from-stamp (org-time &optional end-time-p force)
  "Return time value from org time stamp or range ORG-TIME.
Use the start part of the time range if END-TIME-P is nil.
If ORG-TIME is a single time-stamp and END-TIME-P is non-nil,
return nil; with FORCE return its time value. "
  (or (string-match org-tsr-regexp org-time)
      (error "Wrong org time stamp/range"))
  (if (string-match "---?" org-time)
      (setq org-time
            (if end-time-p
                (substring org-time (match-end 0))
              (substring org-time 0 (match-beginning 0))))
    (and end-time-p (not force)
         (setq org-time nil)))
  (and org-time
       (eval (cons 'encode-time
                   (org-parse-time-string org-time)))))

(defun utl-org-table-beginning-of-section ()
  "Move point to beginning of current section (a space between
horizontal lines) - behaviour is similar to `backward-word' or
`org-table-beginning-of-field'."
  (interactive)
  (let ((cur-col (current-column))
	(beg (org-table-begin)))
    ;; position a point on a proper line
    (if (re-search-backward org-table-hline-regexp beg t)
	(forward-line)
      (org-table-goto-line 1))
    (move-to-column cur-col)))

(defun utl-org-table-next-column ()
  "Move point to first row, next column of the current section"
  (interactive)
  (utl-org-table-beginning-of-section)
  (org-table-next-field))

(defun utl-org-table-kill-rows-recalculate ()
  "Kill all empty rows in the current section and recalculate a
table. Emptiness is checked in the current column after the current
row."
  (interactive)
  (let ((cur-col (org-table-current-column)))
    (save-excursion
      (beginning-of-line)
      (while (and (org-at-table-p)
		  (not (looking-at org-table-hline-regexp)))
	(if (equal "" (org-table-get (org-table-current-line) cur-col))
	    (org-table-kill-row)
	  (forward-line))))
    (org-table-recalculate t)))

(defun utl-org-table-next-table ()
  "Move point to the next org-table in the current buffer"
  (interactive)
  (beginning-of-line)
  (and (utl-re-search-forward "^[^|]")
       (utl-re-search-forward "^|")
       (org-table-goto-line (+ 1 (org-table-current-line)))))


;;; EMMS links

;; To add a possibility of making org links for emms tracks under the
;; point in `emms-playlist-mode' use the following lines:

;; (eval-after-load 'org '(org-add-link-type "emms" 'utl-org-emms-open))
;; (add-hook 'org-store-link-functions 'utl-org-emms-store-link)

(defvar utl-org-emms-file-sleep 3
  "Time in seconds after starting to play file before seeking to time.")

(defvar utl-org-emms-url-sleep 7
  "Time in seconds after starting to play url before seeking to time.")

;;;###autoload
(defun utl-org-emms-open (link)
  "Open emms LINK."
  (let ((path link)
        sec)
    (if (string-match "::\\([0-9]+\\)\\'" link)
        (setq sec (string-to-number (match-string 1 link))
              path (substring link 0 (match-beginning 0))))
    ;; Don't reload a track (just seek to time) if we want to open a
    ;; link with the currently playing track.
    (if (and (fboundp 'emms-track-name)
             (string= path
                      (emms-track-name
                       (emms-playlist-current-selected-track))))
        (emms-start)
      ;; TODO Use some emacs variable for matching url (there is
      ;; `ffap-url-regexp' but it can be modified by a user).
      (if (string-match "^\\(ftp\\|https?\\)://" path)
          (progn (emms-play-url path)
                 ;; We need to wait while the backend will start to play.
                 (and sec (sleep-for utl-org-emms-url-sleep)))
        (emms-play-file path)
        (and sec (sleep-for utl-org-emms-file-sleep))))
    (and sec (emms-seek-to sec))))

;;;###autoload
(defun utl-org-emms-store-link ()
  "Store link for the current playing file in EMMS."
  (when (eq major-mode 'emms-playlist-mode)
    (let ((link (utl-org-emms-make-link
                 (emms-playlist-track-at (point)))))
      (org-store-link-props
       :type        "emms"
       :link        (car link)
       :description (cdr link)))))

(defun utl-org-emms-make-link (&optional track)
  "Return org link for the EMMS track TRACK or current track.
The return value is a cons cell (link . description)."
  (or track
      (setq track (emms-playlist-current-selected-track))
      (error "Couldn't find a track"))
  (let ((path (emms-track-simple-description track))
        (desc (emms-info-track-description track))
        (sec (and (bound-and-true-p emms-playing-time-p)
                  (/= 0 emms-playing-time)
                  emms-playing-time)))
    (cons (concat "emms:" path
                  (and sec (concat "::" (number-to-string sec))))
          ;; if description is the same as path, do not add it
          (unless (string= path desc) desc))))

(provide 'al-org)

;;; al-org.el ends here
