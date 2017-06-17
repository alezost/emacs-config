;;; al-emms-mpv.el --- Additional functionality for using EMMS with mpv  -*- lexical-binding: t -*-

;; Copyright © 2015–2017 Alex Kost

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

(require 'emms-player-simple-mpv)

(defun al/emms-mpv-playing-radio? ()
  "Return non-nil, if current player is 'mpv' and current track
type is 'url' or 'streamlist'."
  (and emms-player-playing-p
       (eq (emms-player-get emms-player-playing-p 'start)
           'emms-player-mpv-start)
       (memq (emms-track-get (emms-playlist-current-selected-track)
                             'type)
             '(url streamlist))))

(defun al/emms-mpv-call-with-property (property function &optional fallback)
  "Call FUNCTION on the value of PROPERTY of the current mpv track.
If there is no such PROPERTY, call FALLBACK function without arguments."
  (emms-player-simple-mpv-tq-enqueue
   (list "get_property" property)
   nil
   (lambda (_ answer)
     (if (emms-player-simple-mpv-tq-success-p answer)
         (funcall function
                  (emms-player-simple-mpv-tq-assq-v 'data answer))
       (if fallback
           (funcall fallback)
         (message "mpv refuses to return '%s' property" property))))))

(defun al/emms-mpv-call-with-metadata (function)
  "Call FUNCTION on the metadata of the current mpv track."
  (al/emms-mpv-call-with-property
   "metadata"
   (lambda (value)
     (funcall function value))))

(defun al/emms-mpv-run-command (command &optional closure function)
  "Run mpv COMMAND for the current EMMS mpv process.
This is a wrapper for `emms-player-simple-mpv-tq-enqueue' (just
to make CLOSURE and FUNCTION optional arguments).

Command is what may be put in mpv conf-file, except it
should be a list of values, e.g.:

  (\"cycle\" \"mute\")
  (\"show_text\" \"${playback-time}\")
  (\"add\" \"speed\" 0.2)"
  (emms-player-simple-mpv-tq-enqueue
   ;; OSD prefixes are disabled for JSON API by default:
   ;; <https://github.com/mpv-player/mpv/issues/4517>.
   (cons "osd-auto" command)
   closure (or function #'ignore)))

(defun al/emms-mpv-tq-enqueue-sync (com-ls closure fn
                                           &optional delay-question)
  "Like `emms-player-simple-mpv-tq-enqueue' but synchronized.
I.e., wait for the result of FN and return it."
  (let (done result)
    (emms-player-simple-mpv-tq-enqueue
     com-ls closure
     (lambda (clos ans)
       (setq result (funcall fn clos ans)
             done t))
     delay-question)
    (while (not done)
      (sleep-for 0.1))
    result))

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
                   (cl-remove-if (lambda (elt)
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

(defun al/emms-mpv-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (al/emms-mpv-run-command '("cycle" "fullscreen")))

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

(define-emms-simple-player-mpv mpv
  '(file url streamlist playlist)
  (concat "\\`\\(http\\|mms\\)://\\|"
          (emms-player-simple-regexp
           "ogg" "mp3" "wav" "mpg" "mpeg" "wmv" "wma"
           "mov" "avi" "divx" "oga" "ogm" "ogv" "asf" "mkv"
           "rm" "rmvb" "mp4" "flac" "vob" "m4a" "ape"
           "flv" "webm"))
  "mpv" "--no-terminal")

(emms-player-simple-mpv-add-to-converters
 'emms-player-mpv "." '(playlist)
 (lambda (track-name)
   (format "--playlist=%s" track-name)))

(provide 'al-emms-mpv)

;;; al-emms-mpv.el ends here
