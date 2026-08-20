;;; al-emms-cmd.el --- Additional commands for `emms' package  -*- lexical-binding: t -*-

;; Copyright © 2013–2026 Alex Kost

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

;; This file contains "entry point" commands for `emms' package to avoid
;; recursive loading.  See `al-eshell-cmd' commentary for details.

;;; Code:

(eval-when-compile
  (require 'let-macros))

(require 'emms)
(require 'al-buffer)
(require 'al-emms)

(declare-function al/emms-mpv-raise-frame "al-emms-mpv")

;;;###autoload
(defun al/emms-playlist-play (string)
  "Switch to EMMS playlist buffer matching STRING and start/resume playing.
Interactively, prompt for an existing playlist."
  (interactive
   (list (completing-read "Switch to buffer: " (al/emms-all-playlists))))
  (al/display-buffer (al/emms-get-playlist string))
  (when emms-player-playing-p
    (al/emms-mpv-raise-frame))
  (emms-start)
  (when-let ((resume (emms-player-get emms-player-playing-p 'resume)))
    (funcall resume)))

;;;###autoload
(defun al/emms-playlist-select (&optional arg)
  "Prompt for EMMS playlist buffer and switch to it.
If ARG is nil, prompt for a buffer that is already opened.
Otherwise (interactively, with prefix), prompt for any existing
playlist."
  (interactive "P")
  (let* ((names (if arg
                    (al/emms-all-playlists)
                  (mapcar #'buffer-name (al/emms-playlist-buffers))))
         (name (completing-read "Switch to buffer: " names)))
    (al/display-buffer (al/emms-get-playlist name))))

;;;###autoload
(defun al/emms-switch-to-playlist-buffer (&optional arg)
  "Switch to the next EMMS playlist.
If ARG is non-nil, prompt for the playlist."
  (interactive "P")
  (let ((buffers (al/emms-playlist-buffers)))
    (if (or arg
            emms-playlist-buffer-p
            (null buffers))
        (al/rotate-or-select-buffer
         buffers
         "There are no EMMS playlists."
         (when arg "EMMS buffer: "))
      (al/display-buffer emms-playlist-buffer)))
  (set-transient-map al/emms-switch-playlist-map))

(provide 'al-emms-cmd)

;;; al-emms-cmd.el ends here
