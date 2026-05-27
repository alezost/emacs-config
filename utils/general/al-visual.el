;;; al-visual.el --- Additional functionality for themes, faces, fonts, etc.  -*- lexical-binding: t -*-

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

;;; Code:

(require 'seq)
(require 'al-misc)


;;; Themes

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


;;; Faces

(defmacro al/with-face (face &rest body)
  "Propertize string returned by BODY with FACE."
  (declare (indent 1) (debug t))
  `(propertize (progn ,@body) 'face ,face))

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


;; Font Lock

(defun al/add-simple-font-lock-keywords (mode names)
  "Fontify NAMES with `font-lock-keyword-face' in MODE."
  (let ((re (concat "(" (regexp-opt names 'group) "\\_>")))
    (font-lock-add-keywords
     mode `((,re 1 font-lock-keyword-face)))))


;;; Fonts

;; Idea from <http://www.emacswiki.org/emacs/SetFonts>.

(defvar al/font-candidates
  '("Liberation Mono-12" "DejaVu Sans Mono-11" "Terminus-12")
  "List of font names used by `al/first-existing-font'.")

(defun al/first-existing-font (&rest font-names)
  "Return the first existing font from FONT-NAMES.
If FONT-NAMES is nil, use `al/font-candidates'."
  (seq-find (lambda (name)
              (find-font (font-spec :name name)))
            (or font-names al/font-candidates)))

(provide 'al-visual)

;;; al-visual.el ends here
