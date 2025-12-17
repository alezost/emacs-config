;;; al-server.el --- Code for working with Emacs server  -*- lexical-binding: t -*-

;; Copyright © 2014–2025 Alex Kost

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

(require 'server)

(defvar al/server-running? nil
  "The state of the current server.
This variable is set by `al/server-start'.")

;;;###autoload
(defun al/server-start (&optional leave-dead inhibit-prompt)
  "Same as `server-start' but also set `al/server-running?'."
  (interactive "P")
  (server-start leave-dead inhibit-prompt)
  (setq al/server-running? (not leave-dead)))

;;;###autoload
(defun al/server-stop ()
  "Stop the current server."
  (interactive)
  (al/server-start t))

(defun al/server-named-start (&rest names)
  "Start server using the first `server-name' from NAMES.
If there is such server running, try the second name and so on.
If servers with all NAMES are running, do not start the server."
  (if-let* ((name (car names)))
      (if (server-running-p name)
          (apply #'al/server-named-start (cdr names))
        (setq server-name name)
        (al/server-start))
    (setq server-name "server-unused")))

(provide 'al-server)

;;; al-server.el ends here
