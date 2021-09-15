;;; al-org.el --- Additional functionality for org-emms

;; Copyright © 2021 Alex Kost

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

(defun al/org-emms-playlist-play (file)
  "Play emms playlist FILE from `org-mode'.
If link contains a track position, start there.  Otherwise,
playback from the start."
  (let* ((path (split-string file "::"))
	 (file (expand-file-name (car path)))
	 (time (org-emms-time-string-to-seconds (cadr path))))
    (emms-play-playlist file)
    (when time
      (when (> org-emms-delay 0)
        (sleep-for org-emms-delay))
      (emms-seek-to time))))

(org-link-set-parameters
 "emms-pl"
 :follow #'al/org-emms-playlist-play)

(provide 'al-org-emms)

;;; al-org.el ends here
