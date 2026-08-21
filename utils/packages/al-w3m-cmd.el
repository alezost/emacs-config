;;; al-w3m-cmd.el --- Additional commands for `w3m' package  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

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

;; This file contains "entry point" commands for `w3m' package to avoid
;; recursive loading.  See `al-eshell-cmd' commentary for details.

;;; Code:

(require 'w3m)
(require 'al-buffer)

;;;###autoload
(defun al/switch-to-w3m ()
  "Switch to the `w3m' buffer.
Start it if necessary."
  (interactive)
  (al/switch-to-buffer-or-funcall #'w3m-alive-p #'w3m))

(provide 'al-w3m-cmd)

;;; al-w3m-cmd.el ends here
