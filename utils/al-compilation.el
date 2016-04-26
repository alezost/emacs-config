;;; al-compilation.el --- Additional functionality for compilation buffers

;; Copyright © 2015-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(require 'cl-lib)
(require 'notifications)
(require 'al-notification)

(defvar al/compilation-sound-success nil
  "Sound file for a successful compilation.")

(defvar al/compilation-sound-error nil
  "Sound file for a failed compilation.")

(defvar exit-status)

;; Idea from <https://gist.github.com/jwiegley/fd78e747f27a90cb67a2>.
(defun al/compilation-notify (buffer reason)
  "Notify about the ended compilation in BUFFER.
This function is intended to be used in
`compilation-finish-functions'."
  (with-current-buffer buffer
    (unless (eq major-mode 'grep-mode)
      (cl-multiple-value-bind (sound urgency)
          (if (= exit-status 0)
              (list al/compilation-sound-success 'normal)
            (list al/compilation-sound-error 'critical))
        (and sound (al/play-sound sound))
        (notifications-notify
         :urgency urgency
         :title "Compilation finished"
         :body (format (concat "Buffer: %s\n"
                               "Reason: %s")
                       (buffer-name buffer) reason))))))

(provide 'al-compilation)

;;; al-compilation.el ends here
