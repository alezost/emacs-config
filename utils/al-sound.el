;;; al-sound.el --- Playing audio and controlling sound parameters  -*- lexical-binding: t -*-

;; Copyright © 2016 Alex Kost

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


;;; Playing sound

(defvar al/play-sound-program (executable-find "play")
  "Default program for playing a sound.
Used by `al/play-sound'.
If nil, use `play-sound-file'.")

(defvar al/play-sound-args (and al/play-sound-program '("-q"))
  "List of default arguments for `al/play-sound-program'.")

;;;###autoload
(defun al/play-sound (file)
  "Play audio FILE with `al/play-sound-program'."
  (if al/play-sound-program
      (apply #'start-process
             al/play-sound-program nil al/play-sound-program
             (append al/play-sound-args (list file)))
    (with-demoted-errors "ERROR during playing sound: %S"
      (play-sound-file file))))


;;; Setting sound

;; The following code is used to set sound parameters (volume and
;; muteness).  It looks mostly like a wrapper around 'amixer' command,
;; except that 'osd-sound' is called instead.
;;
;; This 'osd-sound' is a simple shell script that sends some Guile
;; expression to Guile-Daemon <https://github.com/alezost/guile-daemon>.
;; 2 things eventually happen: amixer is called and the sound value is
;; displayed in OSD.
;;
;; 'osd-sound' script can be found in my Guile-Daemon config:
;; <https://github.com/alezost/guile-daemon-config/blob/master/scripts/osd-sound>.

(defvar al/sound-program "osd-sound"
  "Name of a program to be called with amixer arguments.")

(defun al/sound-call (&rest args)
  "Execute `al/sound-program' using amixer ARGS."
  (apply #'start-process
         al/sound-program nil al/sound-program args))

;;;###autoload
(defun al/set-sound (&rest args)
  "Set sound value for `Master' simple control.
ARGS are the rest amixer arguments after \"sset Master\"."
  (interactive
   (split-string (read-string (concat al/sound-program " sset Master "))
                 " "))
  (apply #'al/sound-call "sset" "Master" args))

(provide 'al-sound)

;;; al-sound.el ends here
