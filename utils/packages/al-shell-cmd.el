;;; al-shell-cmd.el --- Additional functionality for `shell' package  -*- lexical-binding: t -*-

;; Copyright © 2019–2026 Alex Kost

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

;;; Commentary:

;; This file contains commands that start `shell'.  See the commentary
;; of `al-eshell-cmd' for the reason.

;;; Code:

(require 'shell)
(require 'al-places)
(require 'al-buffer)

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
  "Start shell if needed or switch to the next \\[shell] buffer.
If ARG is non-nil, start a new shell buffer."
  (interactive "P")
  (cond
   (arg (shell (generate-new-buffer-name "*shell*")))
   ((and (derived-mode-p 'shell-mode)
         (null (get-buffer-process (current-buffer))))
    (shell (current-buffer)))
   (t (al/rotate-or-select-buffer
       (al/shell-buffers)
       (lambda ()
         (let ((default-directory al/download-dir))
           (shell)))))))

;;;###autoload
(defun al/switch-to-shell-buffer (&optional arg)
  "Switch to \\[shell] buffer or make it if ARG is non-nil."
  (interactive "P")
  (let ((buffers (al/shell-buffers nil t)))
    (if (and buffers (null arg))
        (al/switch-buffer :prompt "Switch to shell buffer: "
                          :buffers buffers)
      (call-interactively 'shell))))

(provide 'al-shell-cmd)

;;; al-shell-cmd.el ends here
