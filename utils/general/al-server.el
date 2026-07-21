;;; al-server.el --- Code for working with Emacs server  -*- lexical-binding: t -*-

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

;;; Code:

(require 'server)
(require 'seq)
(require 'let-macros)

(defvar al/server-running? nil
  "The state of the current server.
This variable is set by `al/server-start'.")

(defun al/server-name ()
  "Return daemon or server name.
Return nil if server is not started."
  (if-let ((name (daemonp)))
      (if (stringp name)
          name
        server-name)
    (and server-process
         server-name)))

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
  (if-let ((name (car names)))
      (if (server-running-p name)
          (apply #'al/server-named-start (cdr names))
        (setq server-name name)
        (al/server-start))
    (setq server-name "server-unused")))

(defun al/autoload-org-protocol (fun files &rest args)
  "Load `org-protocol' if needed.
`org' is huge and loading it at emacs start is wasteful.  Making this
function an `around' advice for `server-visit-files' makes it possible
to avoid requiring `org-protocol' (thus, the whole `org') in the emacs
config file."
  (if (and (null (featurep 'org-protocol))
           (seq-find (lambda (spec)
                       ;; SPEC is (FILENAME . FILEPOS).
                       (string-match "org-protocol:/" (car spec)))
                     files))
      (if (require 'org-protocol nil t)
          ;; `server-visit-files' can't be called as is here, because
          ;; `org-protocol' has just been loaded and the protocol advice
          ;; is not active yet, so call `server-visit-files' outside
          ;; this body.
          (apply #'run-with-idle-timer .1 nil
                 #'server-visit-files files args)
        (message "`org-protocol' has not been loaded!"))
    (apply fun files args)))

(provide 'al-server)

;;; al-server.el ends here
