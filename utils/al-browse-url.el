;;; al-browse-url.el --- Additional functionality for browsing URLs  -*- lexical-binding: t -*-

;; Copyright © 2013–2026 Alex Kost

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

(require 'browse-url)
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
         (org-read-date nil nil nil "Log date: ")))
  (require 'url-expand)
  (browse-url (url-expand-file-name (concat channel "/" date)
                                    al/irc-log-base-url)))


;;; Additional browsers

(defcustom al/browse-url-program "browser"
  "Shell command name for the default browser."
  :type 'string
  :group 'browse-url)

(defcustom al/browse-url-arguments nil
  "A list of strings to pass to the default browser as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

;;;###autoload
(defun al/browse-url-default (url &rest args)
  "Ask the default browser to load URL."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((url (browse-url-encode-url url))
        (process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "browser " url) nil
	   al/browse-url-program
	   (append al/browse-url-arguments
                   args
                   (list url)))))

;;;###autoload
(defun al/browse-url-tor (url &rest args)
  "Ask the TOR browser to load URL."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((url (browse-url-encode-url url))
        (process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "tor " url) nil
	   "torbrowser"
	   (append args (list url)))))

(provide 'al-browse-url)

;;; al-browse-url.el ends here
