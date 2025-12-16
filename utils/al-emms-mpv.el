;;; al-emms-mpv.el --- Additional functionality for using EMMS with mpv  -*- lexical-binding: t -*-

;; Copyright © 2015–2025 Alex Kost

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

(require 'seq)
(require 'emms-mpv)

(defun al/emms-mpv-playing-radio? ()
  "Return non-nil, if current player is `mpv' and current track
type is `url' or `streamlist'."
  (and emms-player-playing-p
       (eq (emms-player-get emms-player-playing-p 'start)
           'emms-mpv-start)
       (memq (emms-track-get (emms-playlist-current-selected-track)
                             'type)
             '(url streamlist))))

(defun al/emms-mpv-call-with-property (property function &optional fallback)
  "Call FUNCTION on the value of PROPERTY of the current mpv track.
If there is no such PROPERTY, call FALLBACK function without arguments."
  (emms-mpv-cmd
   (list "get_property" property)
   (lambda (value error)
     (if error
         (if fallback
             (funcall fallback)
           (message "mpv refuses to return '%s' property" property))
       (funcall function value)))))

(defun al/emms-mpv-call-with-metadata (function)
  "Call FUNCTION on the metadata of the current mpv track."
  (al/emms-mpv-call-with-property
   "metadata"
   (lambda (value)
     (funcall function value))))

(defun al/emms-mpv-run-command (command &optional handler)
  "Run mpv COMMAND for the current EMMS mpv process.
This is a wrapper for `emms-mpv-cmd'.

Command is what may be put in mpv conf-file, except it
should be a list of values, e.g.:

  (\"cycle\" \"mute\")
  (\"show_text\" \"${playback-time}\")
  (\"add\" \"speed\" 0.2)"
  (emms-mpv-cmd
   ;; OSD prefixes are disabled for JSON API by default:
   ;; <https://github.com/mpv-player/mpv/issues/4517>.
   (cons "osd-auto" command)
   (or handler #'ignore)))

(defun al/emms-mpv-show-property (property)
  "Display PROPERTY of the current TRACK."
  (interactive "smpv property: ")
  (al/emms-mpv-call-with-property property
   (lambda (value)
     (message "mpv %s: %S" property value))))

(declare-function pp-display-expression "pp" (expression buffer-name))

(defun al/emms-mpv-show-metadata ()
  "Display metadata of the current TRACK."
  (interactive)
  (require 'pp)
  (al/emms-mpv-call-with-metadata
   (lambda (data)
     (pp-display-expression data "*EMMS track metadata*"))))

(defun al/emms-mpv-show-radio-description ()
  "Display a message about the current radio (url or streamlist) TRACK."
  (interactive)
  (al/emms-mpv-call-with-metadata
   (lambda (data)
     (let ((name        (cdr (assq 'icy-name data)))
           (title       (cdr (assq 'icy-title data)))
           (description (cdr (assq 'icy-description data))))
       (message
        (mapconcat #'identity
                   ;; Remove nils and empty strings.
                   (seq-remove (lambda (elt)
                                 (or (null elt)
                                     (and (stringp elt)
                                          (string= elt ""))))
                               (list title name description))
                   "\n"))))))

(declare-function al/emms-notify "al-emms-notification" ())

(defun al/emms-mpv-show-progress ()
  "Notify about the current mpv track.
Show progress in the OSD if video is playing, or display
notification for an audio track."
  (interactive)
  (al/emms-mpv-call-with-property
   "video-codec"
   (lambda (_)
     (al/emms-mpv-run-command '("show-progress")))
   (lambda ()
     (require 'al-emms-notification)
     (al/emms-notify))))

(defun al/emms-mpv-show-osd-text (text)
  "Display TEXT in OSD."
  (interactive "sShow OSD text: ")
  (al/emms-mpv-run-command (list "show-text" text)))

(defun al/emms-mpv-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (al/emms-mpv-run-command '("cycle" "fullscreen")))

(defun al/emms-mpv-speed-normal ()
  "Change speed to normal."
  (interactive)
  (al/emms-mpv-run-command
   ;; Unlike "add" or "multiply", "set" requires a string, not a number!
   '("set" "speed" "1")
   (lambda (_value _error)
     (al/emms-mpv-show-property "speed"))))

(defun al/emms-mpv-speed-up (&optional value)
  "Increase playback speed by VALUE (0.1 by default).
Interactively with prefix, prompt for VALUE."
  (interactive
   (when current-prefix-arg
     (list (read-number "Increase speed by: " 0.1))))
  (al/emms-mpv-run-command
   (list "add" "speed" (number-to-string (or value 0.1)))
   (lambda (_value _error)
     (al/emms-mpv-show-property "speed"))))

(defun al/emms-mpv-speed-down (&optional value)
  "Decrease playback speed by VALUE (0.1 by default).
Interactively with prefix, prompt for VALUE."
  (interactive
   (when current-prefix-arg
     (list (read-number "Decrease speed by: " 0.1))))
  (al/emms-mpv-speed-up (- (or value 0.1))))

(defun al/emms-mpv-switch-volume (&optional value)
  "Set volume to VALUE or switch between default values.
Interactively with '\\[universal-argument]' argument, prompt for VALUE.
If prefix argument is numerical, use it for VALUE."
  (interactive
   (when current-prefix-arg
     (list (if (numberp current-prefix-arg)
               current-prefix-arg
             (read-number "Set volume to: " 100)))))
  (al/emms-mpv-run-command
   (if value
       (list "set" "volume" (number-to-string value))
     '("cycle-values" "volume" "50" "90" "130" "170"))
   (lambda (_value _error)
     (al/emms-mpv-show-property "volume"))))

(defvar emms-playing-time)

(defun al/emms-mpv-sync-playing-time ()
  "Synchronize `emms-playing-time' with the real time reported by mpv."
  (interactive)
  (al/emms-mpv-call-with-property
   "time-pos"
   (lambda (value)
     (let ((sec (round value)))
       (message "Old playing time: %d; new time: %d"
                emms-playing-time sec)
       (setq emms-playing-time sec)))))

(provide 'al-emms-mpv)

;;; al-emms-mpv.el ends here
