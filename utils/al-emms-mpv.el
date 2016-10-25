;;; al-emms-mpv.el --- Additional functionality for using EMMS with mpv  -*- lexical-binding: t -*-

;; Copyright © 2015-2016 Alex Kost

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

(defun al/emms-mpv-run-command (command)
  "Run mpv COMMAND for the current EMMS mpv process.
COMMAND is what may be put in mpv conf-file, e.g.: 'cycle mute',
'show_text ${playback-time}', etc."
  (interactive "sRun mpv command: ")
  (when (emms-player-simple-mpv-playing-p)
    (tq-enqueue emms-player-simple-mpv--tq
                (concat command "\n")   ; newline is vital
                "" nil #'ignore)))

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

(defun al/emms-mpv-show-progress ()
  "Show progress in the OSD of the current video."
  (interactive)
  (al/emms-mpv-run-command "show_progress"))

(defun al/emms-mpv-toggle-fullscreen ()
  "Toggle fullscreen."
  (interactive)
  (al/emms-mpv-run-command "cycle fullscreen"))

(defun al/emms-mpv-sync-playing-time ()
  "Synchronize `emms-playing-time' with the real time reported by mpv."
  (interactive)
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "time-pos")
   nil
   (lambda (_ ans-ls)
     (if (emms-player-simple-mpv-tq-success-p ans-ls)
         (let ((sec (round (emms-player-simple-mpv-tq-assq-v
                            'data ans-ls))))
           (message "Old playing time: %d; new time: %d"
                    emms-playing-time sec)
           (setq emms-playing-time sec))
       (message "mpv refuses to report about playing time")))))

(defun al/emms-mpv-call-with-metadata (function)
  "Call FUNCTION on the metadata of the current track."
  (emms-player-simple-mpv-tq-enqueue
   '("get_property" "metadata")
   nil
   (lambda (_ answer)
     (if (emms-player-simple-mpv-tq-success-p answer)
         (funcall function
                  (emms-player-simple-mpv-tq-assq-v 'data answer))
       (message "mpv refuses to return metadata")))))

;;;###autoload
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

(declare-function pp-display-expression "pp" (expression buffer-name))

;;;###autoload
(defun al/emms-mpv-show-metadata ()
  "Display metadata of the current TRACK."
  (interactive)
  (require 'pp)
  (al/emms-mpv-call-with-metadata
   (lambda (data)
     (pp-display-expression data "*EMMS track metadata*"))))

(defun al/emms-mpv-add-simple-player ()
  "Generate `emms-player-mpv' player."
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
     (format "--playlist=%s" track-name))))

(provide 'al-emms-mpv)

;;; al-emms-mpv.el ends here
