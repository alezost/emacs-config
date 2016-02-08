;;; al-net.el --- Additional functionality for network stuff (including `net-utils' package)

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 23 Dec 2013

;;; Code:

(defcustom al/net-hosts
  '("google.com" "ya.ru")
  "List of hosts to choose from for `al/ping' command."
  :group 'net-utils
  :type  '(repeat string))

;;;###autoload
(defun al/ping (host)
  "Ping HOST.
Same as `ping' but interactively complete hosts from `al/net-hosts'."
  (interactive
   (list (ido-completing-read "Ping host: " al/net-hosts)))
  (ping host))

;;;###autoload
(defun al/traceroute (host)
  "Traceroute HOST.
Same as `traceroute' but interactively complete hosts from `al/net-hosts'."
  (interactive
   (list (ido-completing-read "Traceroute host: " al/net-hosts)))
  (traceroute host))


;;; Router log

(defvar al/router-log-path "~/.router-log"
  "Directory with router log-files.")

(defvar al/router-log-format "%Y-%m-%d_%H:%M.log"
  "Format used for the names of saved router log-files.
This variable is passed to `format-time-string' with current time.")

(defvar al/router-log-url "http://192.168.1.1/cgi-bin/ExportSyslog.sh"
  "URL with router log.")

(defvar url-handler-regexp)
(declare-function syslog-mode "syslog-mode")

;;;###autoload
(defun al/router-get-log ()
  "Show a buffer with router log."
  (interactive)
  (require 'url-handlers)
  (let ((file-name-handler-alist
         (cons (cons url-handler-regexp 'url-file-handler)
               file-name-handler-alist)))
    (find-file-literally al/router-log-url)
    (syslog-mode)))

(defun al/router-save-log ()
  "Save current buffer as router log."
  (interactive)
  (write-file
   (expand-file-name (format-time-string al/router-log-format
                                         (current-time))
                     al/router-log-path)))

(provide 'al-net)

;;; al-net.el ends here
