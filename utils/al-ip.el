;;; al-ip.el --- Obtaining external ip address  -*- lexical-binding: t -*-

;; Copyright © 2025 Alex Kost

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

(defvar al/ip-urls
  '("http://ifconfig.me/ip"
    "http://4.ident.me"
    "http://icanhazip.com")
  "List of URLs where your external IP address can be obtained from.")

(defvar url-http-end-of-headers)

(defun al/get-ip (url)
  "Return external IP address with the help of URL.
URL should contain only IP address in its output.
Return nil if IP address is not obtained."
  (when-let* ((buffer (url-retrieve-synchronously url)))
    (with-current-buffer buffer
      (goto-char url-http-end-of-headers)
      (skip-chars-forward " \t\n")
      (when (re-search-forward "[0-9.]+" nil t)
        (match-string 0)))))

;;;###autoload
(defun al/ip (url)
  "Get external IP address from URL and report about it."
  (interactive
   (list (completing-read "URL to get IP address from: " al/ip-urls)))
  (let ((url-str (propertize url 'face 'link)))
    (if-let* ((ip (al/get-ip url)))
        (message "IP address reported by <%s>: [%s]."
                 url-str (propertize ip 'face 'bold))
      (message "Cannot retrieve IP address from <%s>." url-str))))

(provide 'al-url)

;;; al-url.el ends here
