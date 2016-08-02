;;; al-sound.el --- Playing audio and controlling sound parameters

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


(provide 'al-sound)

;;; al-sound.el ends here
