;;; al-org.el --- Additional functionality for org-emms  -*- lexical-binding: t -*-

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

;;; Code:

(require 'emms)
(require 'org-emms)
(require 'emms-source-playlist)

(defun al/org-emms-play (file)
  "Play multimedia FILE from `org-mode'.
This function is a substitution for `org-emms-play'."
  (let* ((path (split-string file "::"))
	 (file (expand-file-name (car path)))
	 (time (org-emms-time-string-to-seconds (cadr path))))
    ;; If we want to open a link with the current track, then
    ;; start it if it is stopped or just seek to time, otherwise.
    (if (string= file
                 (emms-track-name
                  (emms-playlist-current-selected-track)))
        (unless emms-player-playing-p
          (emms-start))
      (emms-play-file file))
    (when time
      (and (> org-emms-delay 0)
           (sleep-for org-emms-delay))
      (emms-seek-to time))))


;;; Playlist

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
                        (org-emms-time-string-to-seconds time)))))
      (when (and (null buf)     ; playlist did not exist
                 (or (null time)
                     (/= 0 time)))
        (emms-start)
        (when (and time (/= 0 time))
          (when (> org-emms-delay 0)
            (sleep-for org-emms-delay))
          (emms-seek-to time))))))

(org-link-set-parameters
 "emms-pl"
 :follow #'al/org-emms-playlist-play)

(provide 'al-org-emms)

;;; al-org.el ends here
