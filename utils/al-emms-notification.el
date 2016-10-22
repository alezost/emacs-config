;;; al-emms-notification.el --- EMMS notifications

;; Copyright © 2013-2016 Alex Kost

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

(require 'emms)
(require 'emms-state)
(require 'notifications)
(require 'xml)

(defvar al/emms-notification-artist-format "%s")
(defvar al/emms-notification-title-format "%s")
(defvar al/emms-notification-album-format "%s")
(defvar al/emms-notification-year-format "%s")

(defun al/emms-notification-track-property
    (track property &optional format-str)
  "Return TRACK PROPERTY formatted with FORMAT-STR."
  (let* ((val (emms-track-get track property))
         (val (and (stringp val)
                   (xml-escape-string val))))
    (and val
         (if format-str
             (format format-str val)
           val))))

(defun al/emms-notification-track-description (track)
  "Return description of TRACK suitable for (dunst) notifications."
  (let ((artist   (al/emms-notification-track-property
                   track 'info-artist
                   al/emms-notification-artist-format))
        (title    (al/emms-notification-track-property
                   track 'info-title
                   al/emms-notification-title-format))
        (tracknum (al/emms-notification-track-property
                   track 'info-tracknumber))
        (album    (al/emms-notification-track-property
                   track 'info-album
                   al/emms-notification-album-format))
        (year     (al/emms-notification-track-property
                   track 'info-year
                   al/emms-notification-year-format)))
    (let* ((title (or title
                      (emms-track-simple-description track)))
           (title (if tracknum
                      (format "%02d. %s"
                              (string-to-number tracknum) title)
                    title))
           (album (cond ((and album year)
                         (format "%s – %s" year album))
                        (year  (format "%s" year))
                        (album (format "%s" album)))))
      (mapconcat #'identity
                 (delq nil (list artist title album))
                 "\n"))))

;;;###autoload
(defun al/emms-notify ()
  "Notify about the current track using `notifications-notify'."
  (interactive)
  (let ((track (emms-playlist-current-selected-track)))
    (when track
      (let ((state (emms-state))
            (time (concat emms-state-current-playing-time
                          (and emms-state-total-playing-time
                               (concat " ("
                                       emms-state-total-playing-time
                                       ")")))))
        (notifications-notify
         :app-name "emms"
         :title (format "%s  %s" state time)
         :body (al/emms-notification-track-description track))))))

;;;###autoload
(define-minor-mode al/emms-notification-mode
  "Minor mode for EMMS notifications."
  :global t
  :init-value nil
  (if al/emms-notification-mode
      (add-hook 'emms-player-started-hook 'al/emms-notify t)
    (remove-hook 'emms-player-started-hook 'al/emms-notify)))

(provide 'al-emms-notification)

;;; al-emms-notification.el ends here
