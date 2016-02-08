;;; al-net.el --- Additional functionality for network stuff (including `net-utils' package)

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 23 Dec 2013

;;; Code:

(defcustom utl-net-hosts
  '("google.com" "ya.ru")
  "List of hosts to choose from for `utl-ping' command."
  :group 'net-utils
  :type  '(repeat string))

;;;###autoload
(defun utl-ping (host)
  "Ping HOST.
Same as `ping' but interactively complete hosts from `utl-net-hosts'."
  (interactive
   (list (ido-completing-read "Ping host: " utl-net-hosts)))
  (ping host))

;;;###autoload
(defun utl-traceroute (host)
  "Traceroute HOST.
Same as `traceroute' but interactively complete hosts from `utl-net-hosts'."
  (interactive
   (list (ido-completing-read "Traceroute host: " utl-net-hosts)))
  (traceroute host))


;;; Router log

(defvar utl-router-log-path "~/.router-log"
  "Directory with router log-files.")

(defvar utl-router-log-format "%Y-%m-%d_%H:%M.log"
  "Format used for the names of saved router log-files.
This variable is passed to `format-time-string' with current time.")

(defvar utl-router-log-url "http://192.168.1.1/cgi-bin/ExportSyslog.sh"
  "URL with router log.")

(defvar url-handler-regexp)
(declare-function syslog-mode "syslog-mode")

;;;###autoload
(defun utl-router-get-log ()
  "Show a buffer with router log."
  (interactive)
  (require 'url-handlers)
  (let ((file-name-handler-alist
         (cons (cons url-handler-regexp 'url-file-handler)
               file-name-handler-alist)))
    (find-file-literally utl-router-log-url)
    (syslog-mode)))

(defun utl-router-save-log ()
  "Save current buffer as router log."
  (interactive)
  (write-file
   (expand-file-name (format-time-string utl-router-log-format
                                         (current-time))
                     utl-router-log-path)))

(provide 'al-net)

;;; al-net.el ends here
