;;; al-mode-line.el --- Additional functionality for mode-line  -*- lexical-binding: t -*-

;; Copyright © 2014-2025 Alex Kost

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


;;; Mode names

(defun al/mode-line-default-buffer-identification (mode)
  "Set `mode-line-buffer-identification' to the default value for MODE.
Some major modes like to override `mode-line-buffer-identification'.
If you want to force a mode to use the default value, call this function
like this:

  (al/mode-line-default-buffer-identification \\='Man-mode)
  (al/mode-line-default-buffer-identification \\='dired-mode)"
  (let ((hook (intern (concat (symbol-name mode) "-hook"))))
    (add-hook hook
              (lambda ()
                (setq mode-line-buffer-identification
                      (default-value 'mode-line-buffer-identification))))))


;;; Additional info for major modes

;; To see some additional info in the mode line, I add `al/mode-info'
;; to the `mode-line-modes'.

(defvar-local al/mode-info nil
  "Part of mode line with additional info for the current major mode.")
(put 'al/mode-info 'risky-local-variable t)

;;;###autoload
(defun al/mode-ibuffer-info ()
  "Set `al/mode-info' to the additional info for `ibuffer-mode'.
This function is intended to be added to `ibuffer-mode-hook'."
  (setq al/mode-info
        '(""
          (ibuffer-sorting-mode (:eval (symbol-name ibuffer-sorting-mode)))
          (ibuffer-sorting-reversep "|r"))))

(provide 'al-mode-line)

;;; al-mode-line.el ends here
