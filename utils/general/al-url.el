;;; al-url.el --- Code for searching and downloading various stuff  -*- lexical-binding: t -*-

;; Copyright © 2015–2026 Alex Kost

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

(require 'let-macros)
(require 'al-format)
(require 'al-text)

(defvar al/url-regexp
  "https?://"
  "Regexp matching the beginning of an URL.")

(defvar al/url-mp3-regexp
  (rx "http" (? ?s) "://" (1+ any) ".mp3")
  "Regexp for mp3 file.")

(defun al/check-url (value)
  "Check if VALUE is a string matching `al/url-regexp'.
Return matching URL if it is, return nil otherwise."
  (and (stringp value)
       (string-match al/url-regexp value)
       (substring-no-properties value (match-beginning 0))))

(defun al/url-query-parameters (url)
  "Return alist of query parameters from URL.
Return nil, if URL does not contain query parameters."
  (when-let ((query (cadr (split-string url "?"))))
    (url-parse-query-string query)))

(defun al/url-strip-query-parameters (url)
  "Return URL without query parameters.
I.e., without substring starting with \"?\"."
  (if (string-match "\\?" url)
      (substring url 0 (match-beginning 0))
    url))


;;; YouTube URLs

(defvar al/url-youtube-base-url
  "https://www.youtube.com/"
  "YouTube base URL.")

(defun al/url-youtube-video (&optional id time)
  "Return URL for youtube video with ID.
If ID is nil, return an incomplete URL with missing video ID.
TIME can be an integer (number of seconds) or a string with time format
supported by `al/time-string-to-seconds'."
  (concat al/url-youtube-base-url "watch?v=" id
          (and time
               (concat "&t="
                       (number-to-string
                        (if (integerp time)
                            time
                          (al/time-string-to-seconds time)))))))

(defun al/url-youtube-playlist (&optional id)
  "Return URL for youtube playlist with ID.
If ID is nil, return an incomplete URL with missing video ID."
  (concat al/url-youtube-base-url "playlist?list=" id))

(defun al/url-youtube-video-id (url)
  "Return youtube video ID from URL.
Return nil if URL does not contain video ID."
  (when-let ((url    (al/check-url url))
             (params (al/url-query-parameters url))
             (ids    (alist-get "v" params nil nil #'string=)))
    (car ids)))

(defun al/url-youtube-video-id-candidates ()
  "Return list of video IDs from various places."
  (if-letn ((candidates (al/string-candidates nil '(al/url-youtube-video-id)))
            (at-point   (and (require 'al-thingatpt nil t)
                             (thing-at-point 'youtube 'no-properties))))
      (cons at-point candidates)
    candidates))


;;; Other URLs

(defun al/url-candidates (&optional no-query)
  "Return a list of URL candidates from various places.
If NO-QUERY is non-nil, remove query parameters from URLs."
  (let* ((inv-filters '(al/check-url al/first-line))
         (inv-filters (if no-query
                          (cons #'al/url-strip-query-parameters
                                inv-filters)
                        inv-filters))
         (filters (reverse inv-filters)))
    (al/string-candidates 'url filters)))

(defvar al/url-papago-base-url
  "https://papago.naver.com/"
  "Papago base URL.")

(defun al/url-papago (source target text)
  "Return URL to translate TEXT from source language to target using Papago."
  (concat al/url-papago-base-url "?"
          (url-build-query-string
           `((sk ,source)
             (tk ,target)
             (st ,text)))))

(provide 'al-url)

;;; al-url.el ends here
