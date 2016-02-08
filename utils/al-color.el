;;; al-color.el --- Additional functionality for working with color themes, faces, ...

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 8 Sep 2013

;;; Code:


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
  (message "Current theme: '%S'." theme))

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
  (or pos
      (setq pos (point)))
  (or (get-char-property pos 'read-face-name)
      (get-char-property pos 'face)))

;;;###autoload
(defun al/face-to-kill-ring ()
  "Put a name of the current face into kill ring."
  (interactive)
  (let ((face (al/get-face)))
    (if (null face)
        (message "No face at point.")
      (setq face (symbol-name face))
      (kill-new face)
      (message "%s" face))))

(provide 'al-color)

;;; al-color.el ends here
