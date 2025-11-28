;;; al-color.el --- Additional functionality for working with color themes, faces, ...  -*- lexical-binding: t -*-

;; Copyright © 2013-2025 Alex Kost

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

(require 'al-misc)


;;; Managing themes

;;;###autoload
(defun al/load-theme (theme)
  "Similar to `load-theme' except it unloads the current themes at first."
  (interactive
   (list (intern (completing-read
                  "Load custom theme: "
                  (mapcar #'symbol-name (custom-available-themes))))))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme theme t)
  (message "Current theme: `%S'." theme))

;; Idea from <https://gist.github.com/joehakimrahme/6305195>.
;;;###autoload
(defun al/load-random-theme ()
  "Load any random theme from the available ones."
  (interactive)
  (let ((themes (custom-available-themes)))
    (al/load-theme (nth (random (length themes))
                        themes))))


;;; Working with faces

(defun al/get-face (&optional pos)
  "Return name of the face at point POS.
If POS is nil, use current point position."
  (let ((pos (or pos (point))))
    (or (get-char-property pos 'read-face-name)
        (get-char-property pos 'face))))

;;;###autoload
(defun al/face-to-kill-ring ()
  "Put a name of the current face into kill ring."
  (interactive)
  (or (al/with-eval-to-kill-ring (al/get-face))
      (message "No face at point.")))

(provide 'al-color)

;;; al-color.el ends here
