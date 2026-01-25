;;; al-eshell.el --- Additional functionality for eshell  -*- lexical-binding: t -*-

;; Copyright © 2026 Alex Kost

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

(require 'esh-mode)

;; The following code allows to start a new eshell command right after
;; finishing the previous one.  Run `al/eshell-test' in eshell buffer to
;; check this.

(defvar-local al/eshell-next-command nil)

(defun al/eshell-run-next-command (&rest _)
  "Run `al/eshell-next-command' in the current `eshell' buffer."
  (when (and al/eshell-next-command
             (derived-mode-p 'eshell-mode))
    (let ((cmd al/eshell-next-command))
      (setq al/eshell-next-command nil)
      (goto-char (point-max))
      (insert cmd)
      (eshell-send-input))))

(advice-add 'eshell-send-input :after #'al/eshell-run-next-command)

(defun al/eshell-test ()
  (setq al/eshell-next-command
        "sh -c \"echo one ; sleep 1 ; echo two ; sleep 1 ; echo three\"")
  "Starting the next command...")

(provide 'al-eshell)

;;; al-eshell.el ends here
