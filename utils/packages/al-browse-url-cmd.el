;;; al-browse-url-cmd.el --- Additional commands for `browse-url' package  -*- lexical-binding: t -*-

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

;; This file contains "entry point" commands for `browse-url' package to
;; avoid recursive loading.  See `al-eshell-cmd' commentary for details.

;;; Code:

(eval-when-compile
  (require 'let-macros))

(require 'browse-url)
(require 'al-read)
(require 'al-url)

;;;###autoload
(defun al/browse-youtube-video (id &optional time)
  "Browse youtube url for video or playlist with ID.
TIME should be a string, see `al/time-string-to-seconds' for details.
Interactively with arg, prompt for TIME."
  (interactive
   (list (or (and (require 'al-thingatpt nil t)
                  (thing-at-point 'youtube))
             (let ((prompt "YouTube video or playlist ID: ")
                   (ids (al/url-youtube-video-id-candidates)))
               (pcase ids
                 ('() (read-string prompt))
                 (`(,id) id)
                 (_ (completing-read prompt ids)))))
         (and current-prefix-arg
              (read-string "Time stamp: "))))
  (cond
   ((= (length id) 11)
    (browse-url (al/url-youtube-video id time)))
   ((let (case-fold-search)
      (string-match-p "\\`PL" id))
    (browse-url (al/url-youtube-playlist id)))
   (t
    (error "Unknown youtube ID: %s" id))))

(defvar al/urls nil
  "List of URLs for `al/browse-url'.
Each element of the list should be a string of \"<something> http...\"
form.")

;;;###autoload
(defun al/browse-url (url &optional no-query)
  "Browse URL.
Interactively, prompt for URL using completions from clipboard, URL at
point, and `al/urls' list.

If NO-QUERY is non-nil (interactively, with arg), remove query
parameters from URL."
  (interactive
   (let ((url (al/completing-read-no-sort
               "Browse URL: "
               (append (al/url-candidates) al/urls))))
     (list url current-prefix-arg)))
  (if-let ((url (al/check-url url))
           (url (if no-query
                    (al/url-strip-query-parameters url)
                  url)))
      (browse-url url)
    (error "`%s' does not match `al/url-regexp'" url)))


;;; Browse IRC logs from gnunet

;; TODO this bot doesn't exist anymore
(defvar al/irc-log-base-url "https://gnunet.org/bot/log/"
  "Base URL with IRC logs.")

(defvar al/irc-log-channels '("guix" "guile" "gnunet")
  "List of channels that are logged by gnunet bot.")

(declare-function url-expand-file-name "url-expand" t)
(declare-function org-read-date "org" t)

;;;###autoload
(defun al/browse-irc-log (channel &optional date)
  "Browse IRC log of the CHANNEL from DATE."
  (interactive
   (list (completing-read "IRC channel: " al/irc-log-channels nil t)
         (progn
           (require 'org)
           (org-read-date nil nil nil "Log date: "))))
  (require 'url-expand)
  (browse-url (url-expand-file-name (concat channel "/" date)
                                    al/irc-log-base-url)))

(provide 'al-browse-url-cmd)

;;; al-browse-url-cmd.el ends here
