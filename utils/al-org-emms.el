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

(defvar al/org-emms-seek-time nil
  "Time in seconds the lastly loaded mpv file should be seeked to.
If nil, do not perform seeking.")

(defvar al/org-emms-time-format "%m:%.2s"
  "Format string for a track position in org links.
This string is passed to `format-seconds' function.")

(defun al/org-emms-seek ()
  "Seek the current EMMS track to `al/org-emms-seek-time'.
This function is intended to be added to `emms-mpv-file-loaded-hook'."
  (when-let* ((time al/org-emms-seek-time))
    (setq al/org-emms-seek-time nil)
    (emms-player-seek-to time)))

;;;###autoload
(defun al/org-emms-play (file)
  "Play EMMS FILE from `org-mode'."
  (let* ((path (split-string file "::"))
	 (file (expand-file-name (car path)))
         (time (cadr path))
	 (time (and time (al/time-string-to-seconds time)))
         (track (emms-playlist-current-selected-track)))
    (when time
        (setq al/org-emms-seek-time time))
    ;; If we want to open a link with the current track, then
    ;; start it if it is stopped or just seek to time, otherwise.
    (if (string= file (emms-track-name track))
        (if emms-player-playing-p
            (al/org-emms-seek)
          (emms-player-start track))
      (emms-play-file file))))

(defun al/org-emms-make-link (track &optional time)
  "Return org link for EMMS TRACK.
TIME is seeking time in seconds."
  (concat "emms:" (emms-track-name track)
          (and time (/= 0 time)
               (concat "::" (format-seconds al/org-emms-time-format
                                            time)))))

;;;###autoload
(defun al/org-emms-store-link ()
  "Store org link for the current playing track in EMMS."
  (interactive)
  (when (derived-mode-p '(emms-playlist-mode emms-browser-mode))
    (if emms-player-playing-p
        (al/emms-mpv-call-with-property
         "time-pos"
         ;; This is performed asynchronously, so save the current (EMMS
         ;; playlist) buffer to get track and playing time from there.
         (let ((buf (current-buffer)))
           (lambda (sec)
             (with-current-buffer buf
               (setq emms-playing-time (round sec))
               (org-link--add-to-stored-links
                (al/org-emms-make-link (emms-playlist-selected-track)
                                       emms-playing-time)
                nil)))))
      (org-link--add-to-stored-links
       (al/org-emms-make-link (emms-playlist-selected-track))
       nil))))


;;; "emms-pl" links

;;;###autoload
(defun al/org-emms-playlist-play (link)
  "Add or play EMMS playlist from `org-mode' LINK.
LINK has \"FILE[::TIME]\" form, where FILE is a playlist file and TIME
is the track position in seconds.

EMMS playlist buffer has the same name as FILE basename.  If it already
exists or if TIME is not specified, just add playlist but don't play it.

If TIME is specified, start playback from this position.
If TIME is zero, play from the beginning."
  (let* ((path (split-string link "::"))
	 (file (expand-file-name (car path)))
         (buf-name (file-name-base file))
         (buf (get-buffer buf-name)))
    (setq emms-playlist-buffer
          (or buf (emms-playlist-new buf-name)))
    (if buf
        (progn
          (switch-to-buffer buf)
          (message "Playlist %S already exists." buf-name))
      (emms-add-playlist file)
      (if-let* ((time (cadr path))
                (time (and (not (equal "" time))
                           (al/time-string-to-seconds time))))
          (progn
            (when (< 0 time)
              (setq al/org-emms-seek-time time))
            (emms-start))
        (switch-to-buffer emms-playlist-buffer)))))

(provide 'al-org-emms)

;;; al-org-emms.el ends here
