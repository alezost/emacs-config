;;; al-sr.el --- Additional functionality for Sunrise Commander

;; Copyright © 2012-2016 Alex Kost

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

(require 'sunrise-commander)

;;;###autoload
(defun al/sr-toggle ()
  "Toggle Sunrise Commander.
If sr is active - show dired with current directory.
If dired is active - show sr with current directory."
  (interactive)
  (let (file)
    (if sr-running
        (progn (setq sr-running nil)
               (delete-other-windows)
               (and (eq major-mode 'sr-mode)
                    (setq file (dired-get-file-for-visit))
                    (dired (dired-current-directory))
                    (dired-goto-file file)))
      (and (eq major-mode 'dired-mode)
           (setq file (dired-get-file-for-visit)))
      (sunrise)
      (and file (sr-dired file)))))

(provide 'al-sr)

;;; al-sr.el ends here
