;;; al-ido.el --- Additional functionality for ido-mode

;; Copyright © 2013–2017 Alex Kost

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

(require 'ido)

(defvar ido-rotate-temp)

;;;###autoload
(defun al/ido-set-current-directory (dir)
  "Change the current ido working directory to DIR."
  (interactive)
  (ido-set-current-directory dir)
  (setq ido-exit 'refresh)
  (setq ido-text-init ido-text)
  (setq ido-rotate-temp t)
  (exit-minibuffer))

;;;###autoload
(defun al/ido-copy-current-item ()
  "Put the current ido item into `kill-ring'."
  (interactive)
  (kill-new (car ido-matches)))

(provide 'al-ido)

;;; al-ido.el ends here
