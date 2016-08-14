;;; al-emms.el --- Additional functionality for EMMS

;; Copyright © 2013-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'emms)

(defun al/emms-seek-forward (seconds)
  "Seek by SECONDS forward.
Interactively, define SECONDS with a numeric prefix."
  (interactive "p")
  (when emms-player-playing-p
    (emms-player-seek seconds)))

(defun al/emms-seek-backward (seconds)
  "Seek by SECONDS backward.
Interactively, define SECONDS with a numeric prefix."
  (interactive "p")
  (al/emms-seek-forward (- seconds)))

(defun al/emms-seek-to (seconds)
  "Seek the current player to SECONDS.
Interactively, prompt for the number of minutes.
With prefix, prompt for the number of seconds."
  (interactive
   (list (if current-prefix-arg
             (read-number "Seconds to seek to: ")
           (* 60 (read-number "Minutes to seek to: ")))))
  (emms-seek-to seconds))

(defun al/emms-source-add-and-play (source &rest args)
  "Add the tracks of SOURCE to EMMS playlist and play the first one."
  (with-current-emms-playlist
    (goto-char (point-max))
    (let ((first-new-track (point)))
      (apply #'emms-playlist-insert-source source args)
      (emms-playlist-select first-new-track)))
  (emms-stop)
  (emms-start))

(defun al/emms-first ()
  "Start playing the first track in the EMMS playlist."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-current-select-first)
  (emms-start))


;;; Track description

(defun al/emms-full-track-description (track)
  "Return a full description of TRACK.
Intended to be used for `emms-track-description-function'."
  (let ((artist   (emms-track-get track 'info-artist))
        (title    (emms-track-get track 'info-title))
        (tracknum (emms-track-get track 'info-tracknumber))
        (album    (emms-track-get track 'info-album))
        (year     (emms-track-get track 'info-year)))
    (let ((name (cond
                 ((and artist title) (concat artist " – " title))
                 (title title))))
      (if (null name)
          (emms-track-simple-description track)
        (when tracknum
          (setq name (format "%02d. %s" (string-to-number tracknum) name)))
        (cond
         ((and album year)
          (setq name (format "%s [%s – %s]" name year album)))
         (year
          (setq name (format "%s [%s]" name year)))
         (album
          (setq name (format "%s [%s]" name album))))
        name))))

(defun al/emms-short-track-description (track)
  "Return a short description of TRACK suitable for mode-line."
  (let ((title  (emms-track-get track 'info-title)))
    (if title
        title
      (let ((type (emms-track-type track)))
        (cond ((eq 'file type)
               (file-name-nondirectory (emms-track-name track)))
              ((eq 'url type)
               (url-file-nondirectory (emms-format-url-track-name
                                       (emms-track-name track))))
              (t (concat (symbol-name type)
                         ": " (emms-track-name track))))))))


;;; Notifications

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


;;; Mode line

(defvar al/emms-mode-line-song-function
  'al/emms-short-track-description
  "Default function used in `al/emms-mode-line-song-string'.")

(defun al/emms-mode-line-song-string ()
  "Format the currently playing song.
Intended to be used for `emms-mode-line-mode-line-function'."
  (format emms-mode-line-format
          (funcall al/emms-mode-line-song-function
                   (emms-playlist-current-selected-track))))


;;; Misc

(declare-function wget "wget" t)

;;;###autoload
(defun al/emms-playlist-wget ()
  "Run `wget' on the URL track at point."
  (interactive)
  (let* ((track (emms-playlist-track-at))
         (type  (emms-track-get track 'type))
         (url   (emms-track-get track 'name)))
    (unless (eq type 'url)
      (user-error "Current track is not of 'url' type."))
    (wget url)))

(provide 'al-emms)

;;; al-emms.el ends here
