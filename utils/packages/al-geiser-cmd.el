;;; al-geiser-cmd.el --- Additional commands for `geiser' package  -*- lexical-binding: t -*-

;; Copyright © 2016–2026 Alex Kost

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

;; This file contains "entry point" commands for `geiser' package to
;; avoid recursive loading.  See `al-eshell-cmd' commentary for details.

;;; Code:

(require 'geiser-mode)
(require 'al-geiser)

;;;###autoload
(defun al/geiser-guile-switch-current-window (arg)
  "Switch to a running guile REPL, or start one.
This is the same as `geiser-guile-switch' except it always use the
current window ignoring the value of `geiser-repl-use-other-window'."
  (interactive "P")
  (let (geiser-repl-use-other-window)
    (geiser-repl-switch arg 'guile)))

;;;###autoload
(defun al/geiser-socket-connect (socket)
  "Connect Geiser to Guile's SOCKET file.
Interactively, prompt for SOCKET using completions from
`al/geiser-sockets'."
  (interactive
   (list (expand-file-name
          (completing-read "Socket: " al/geiser-sockets))))
  (geiser-connect-local 'guile socket))

(provide 'al-geiser-cmd)

;;; al-geiser-cmd.el ends here
