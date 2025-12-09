;;; al-org-emms.el --- Functionality to use org links for EMMS tracks and playlists  -*- lexical-binding: t -*-

;; Copyright © 2021–2025 Alex Kost

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

;;; Commentary:

;; Most of the code originates from `org-emms' package.

;;; Code:

(require 'ol)
(require 'emms)
(require 'emms-source-playlist)
(require 'emms-playing-time)
(require 'al-misc)
(require 'al-emms-mpv)


;;; "emms" links

;; TODO Remove this variable and use mpv "file-loaded" event instead.
(defvar al/org-emms-delay 2
  "Time in seconds between starting playing and seeking to time.")

(defvar al/org-emms-time-format "%m:%.2s"
  "Format string for a track position in org links.
This string is passed to `format-seconds' function.")

;;;###autoload
(defun al/org-emms-play (file)
  "Play EMMS FILE from `org-mode'."
  (let* ((path (split-string file "::"))
	 (file (expand-file-name (car path)))
	 (time (al/time-string-to-seconds (cadr path))))
    ;; If we want to open a link with the current track, then
    ;; start it if it is stopped or just seek to time, otherwise.
    (if (string= file
                 (emms-track-name
                  (emms-playlist-current-selected-track)))
        (unless emms-player-playing-p
          (emms-start))
      (emms-play-file file))
    (when time
      (and (> al/org-emms-delay 0)
           (sleep-for al/org-emms-delay))
      (emms-seek-to time))))

(defun al/org-emms-make-link ()
  "Return org link for the the current EMMS track."
  (al/emms-mpv-sync-playing-time)
  ;; TODO use `al/emms-mpv-call-with-property' instead of sleeping
  (sleep-for 1)
  (let ((track (emms-playlist-selected-track)))
    (concat "emms:" (emms-track-name track)
            (and (/= 0 emms-playing-time)
                 (concat "::"
                         (format-seconds al/org-emms-time-format
                                         emms-playing-time))))))

;;;###autoload
(defun al/org-emms-store-link ()
  "Store org link for the current playing track in EMMS."
  (when (derived-mode-p '(emms-playlist-mode emms-browser-mode))
    (let ((link (al/org-emms-make-link)))
      (org-link-store-props
       :type "emms"
       :link link))))


;;; "emms-pl" links

;;;###autoload
(defun al/org-emms-playlist-play (link)
  "Add or play EMMS playlist from `org-mode' LINK.
LINK has \"FILE[::TIME]\" form, where FILE is a playlist file and TIME
is the track position in seconds.

EMMS playlist buffer has the same name as FILE basename.
If it already exists, just add playlist but don't play it.

If non-zero TIME is specified, start playback from this position.
If TIME is zero, just add playlist but don't play it.
If TIME is not specified, play the playlist from the start."
  (let* ((path (split-string link "::"))
	 (file (expand-file-name (car path)))
         (buf-name (file-name-base file))
         (buf (get-buffer buf-name)))
    (setq emms-playlist-buffer
          (or buf (emms-playlist-new buf-name)))
    (emms-add-playlist file)
    (let* ((time (cadr path))
           (time (and time
                      (if (equal "" time)
                          nil
                        (al/time-string-to-seconds time)))))
      (when (and (null buf)     ; playlist did not exist
                 (or (null time)
                     (/= 0 time)))
        (emms-start)
        (when (and time (/= 0 time))
          (when (> al/org-emms-delay 0)
            (sleep-for al/org-emms-delay))
          (emms-seek-to time))))))

(provide 'al-org-emms)

;;; al-org-emms.el ends here
