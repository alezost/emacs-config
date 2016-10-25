;;; al-emms.el --- Additional functionality for EMMS

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

(declare-function al/emms-mpv-playing-radio? "al-emms-mpv" ())
(declare-function al/emms-mpv-show-radio-description "al-emms-mpv" (track))
(declare-function al/emms-mpv-show-metadata "al-emms-mpv" (track))

;;;###autoload
(defun al/emms-show (&optional arg)
  "Describe the current EMMS track in the minibuffer.
If ARG is specified, show metadata of the track."
  (interactive "P")
  (require 'al-emms-mpv)
  (cond (arg
         (al/emms-mpv-show-metadata))
        ((al/emms-mpv-playing-radio?)
         (al/emms-mpv-show-radio-description))
        (t
         (message (format emms-show-format
                          (emms-track-description
                           (emms-playlist-current-selected-track)))))))


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

(defvar al/emms-split-track-regexp
  (rx (group (+? any))
      " - "
      (group (+ any)))
  "Regexp used by `al/emms-split-track-name'.")

(defun al/emms-split-track-name (name)
  "Assuming NAME is \"ARTIST - TITLE\" string, return (ALIST TITLE) list."
  (string-match al/emms-split-track-regexp name)
  (list (match-string 1 name)
        (match-string 2 name)))

(provide 'al-emms)

;;; al-emms.el ends here
