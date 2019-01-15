;;; al-shell.el --- Additional functionality for `shell'

;; Copyright © 2019 Alex Kost

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

(require 'shell)
(require 'al-buffer)
(require 'al-misc)

(defun al/shell-buffers (&optional all no-sort)
  "Return a list of active shell buffers.
If ALL is non-nil, return a list of all (including non-active)
shell buffers.
If NO-SORT is non-nil, do not sort the list by buffer names."
  (al/buffers
   (lambda (buf)
     (with-current-buffer buf
       (and (derived-mode-p 'shell-mode)
            (or all
                (get-buffer-process buf)))))
   (unless no-sort #'al/buffer-name<)))

;;;###autoload
(defun al/shell (&optional arg)
  "Start shell if needed or switch to \\[shell] buffer.
Interactively, ARG has the same meaning as in `shell'."
  (interactive "P")
  (if arg
      (call-interactively 'shell)
    (if (derived-mode-p 'shell-mode)
        (let ((buf (current-buffer)))
          (if (get-buffer-process buf)
              (switch-to-buffer (al/next-element (al/shell-buffers) buf))
            (shell buf)))
      (let ((buf (al/next-element (al/shell-buffers))))
        (if buf
            (switch-to-buffer buf)
          (call-interactively 'shell))))))

;;;###autoload
(defun al/switch-to-shell-buffer ()
  "Switch to \\[shell] buffer or make it if needed."
  (interactive)
  (let ((buffers (al/shell-buffers nil t)))
    (if buffers
        (al/switch-buffer "Switch to shell buffer: "
                          :buffers (mapcar #'buffer-name buffers))
      (call-interactively 'shell))))

(provide 'al-shell)

;;; al-shell.el ends here
