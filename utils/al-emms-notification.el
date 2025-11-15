;;; al-emms-notification.el --- EMMS notifications  -*- lexical-binding: t -*-

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

(eval-when-compile (require 'cl-lib))
(require 'emms)
(require 'emms-state)
(require 'notifications)
(require 'xml)
(require 'al-emms)
(require 'al-emms-mpv)

(defvar al/emms-notification-artist-format "%s")
(defvar al/emms-notification-title-format "%s")
(defvar al/emms-notification-album-format "%s")
(defvar al/emms-notification-year-format "%s")

(defun al/emms-notification-format (value &optional format-str)
  "Return VALUE (string or nil) formatted with FORMAT-STR."
  (when (stringp value)
    (let ((val (xml-escape-string value)))
      (if format-str
          (format format-str val)
        val))))

(defun al/emms-notification-track-name (&optional artist title album
                                                  year track-number)
  "Return track description suitable for (dunst) notifications."
  (let ((artist   (al/emms-notification-format
                   artist al/emms-notification-artist-format))
        (title    (al/emms-notification-format
                   title al/emms-notification-title-format))
        (album    (al/emms-notification-format
                   album al/emms-notification-album-format))
        (year     (al/emms-notification-format
                   year al/emms-notification-year-format)))
    (let ((title (if track-number
                     (format "%02d. %s"
                             (string-to-number track-number) title)
                   title))
          (album (cond ((and album year)
                        (format "%s – %s" year album))
                       (year  (format "%s" year))
                       (album (format "%s" album)))))
      (mapconcat #'identity
                 (delq nil (list artist title album))
                 "\n"))))

(defun al/emms-notification-radio-description (metadata)
  "Return description of TRACK ."
  (cl-multiple-value-bind (artist title)
      (al/emms-split-track-name (cdr (assq 'icy-title metadata)))
    (al/emms-notification-track-name
     artist title (cdr (assq 'icy-name metadata)))))

(defun al/emms-notification-track-description (track)
  "Return description of TRACK suitable for (dunst) notifications."
  (al/emms-notification-track-name
   (emms-track-get track 'info-artist)
   (or (emms-track-get track 'info-title)
       (emms-track-simple-description track))
   (emms-track-get track 'info-album)
   (emms-track-get track 'info-year)
   (emms-track-get track 'info-tracknumber)))

(defun al/emms-notification-notify (state time string)
  "Notify about STATE, TIME and STRING using `notifications-notify'."
  (notifications-notify
   :app-name "emms"
   :title (format "%s  %s" state time)
   :body string))

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
        (if (al/emms-mpv-playing-radio?)
            (al/emms-mpv-call-with-metadata
             (lambda (data)
               (al/emms-notification-notify
                state time
                (al/emms-notification-radio-description data))))
          (al/emms-notification-notify
           state time
           (al/emms-notification-track-description track)))))))

;;;###autoload
(define-minor-mode al/emms-notification-mode
  "Minor mode for EMMS notifications."
  :global t
  :group 'al/emms-notification
  :init-value nil
  (if al/emms-notification-mode
      (add-hook 'emms-player-started-hook 'al/emms-notify t)
    (remove-hook 'emms-player-started-hook 'al/emms-notify)))

(provide 'al-emms-notification)

;;; al-emms-notification.el ends here
