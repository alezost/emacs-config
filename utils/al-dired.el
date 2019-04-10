;;; al-dired.el --- Additional functionality for dired

;; Copyright © 2012–2019 Alex Kost

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

(require 'dired)
(require 'al-process)


;;; Processes

;;;###autoload
(defun al/dired-start-process (program &optional args)
  "Open current file with a PROGRAM."
  ;; Shell command looks like this: "program [ARGS]... FILE" (ARGS can
  ;; be nil, so remove it).
  (apply #'al/start-process
         program
         (remove nil (list args (dired-get-file-for-visit)))))

;;;###autoload
(defun al/dired-start-process-on-marked-files (program &optional args)
  "Open marked files with a PROGRAM."
  (apply #'al/start-process
         program
         (remove nil (append args (dired-get-marked-files)))))


;;; Mode line

(defvar al/mode-info)

(defun al/dired-sort-set-mode-line ()
  "Replacement for `dired-sort-set-mode-line'."
  (when (eq major-mode 'dired-mode)
    (setq al/mode-info
	  (let (case-fold-search)
            (cond ((string-match-p
                    dired-sort-by-name-regexp dired-actual-switches)
                   "name")
                  ((string-match-p
                    dired-sort-by-date-regexp dired-actual-switches)
                   "date")
                  (t dired-actual-switches))))))

(provide 'al-dired)

;;; al-dired.el ends here
