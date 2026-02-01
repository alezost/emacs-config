;;; al-ytdl.el --- Additional functionality for ytdl  -*- lexical-binding: t -*-

;; Copyright © 2026 Alex Kost

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
;; along with this program.  If not, see <http://www.gnu.ytdl/licenses/>.

;;; Commentary:

;; XXX This file was "work in progress" until I realized that I don't
;; like how `ytdl' works.  I prefer to see the downloading progress
;; instead of having it somewhere in the background without being able
;; to check the current downloading speed, how many is already
;; downloaded, how many left, etc.  So instead, I decided to use eshell
;; buffers to run yt-dlp commands there (see `al/eshell-ytdlp').

;;; Code:

(require 'ytdl)
(require 'let-macros)
(require 'al-places)
(require 'al-text)
(require 'al-url)

(defvar al/ytdl-args-alist nil
  "Alist of additional arguments for `al/ytdl-download'.
Each element has (NAME . ARGS) form, where NAME should be a symbol, and
ARGS is either a string or a list of strings with command line
arguments.")

(defvar-local al/ytdl-url nil
  "URL of the downloaded video.")

(defvar-local al/ytdl-args nil
  "Arguments for the current `ytdl-command' call.")

(defvar-local al/ytdl-file-name nil
  "Original file name for the current URL.")

(defun al/ytdl-arg-list (args)
  "Return a list of command line arguments from ARGS.

ARGS can be one of the following:

  - a list of strings with command line arguments,

  - a string with arguments separated by spaces,

  - a symbol from `al/ytdl-args-alist'."
  (pcase args
    ((pred listp)   args)
    ((pred stringp) (split-string args))
    ((pred symbolp) (al/ytdl-arg-list
                     (alist-get args al/ytdl-args-alist)))))

(defun al/ytdl-file-name-sentinel (process _event)
  "Sentinel for `ytdl-command' process called to define a video file name."
  (let ((status (process-status process))
        (buf    (process-buffer process)))
    (message "-- yt-fn status: %s" status)
    (when (eq status 'exit)
      (with-current-buffer buf
        (goto-char (point-min))
        (if (looking-at "ERROR")
            (message (concat "ERROR al/ytdl: Cannot get file name:\n"
                             (buffer-substring-no-properties
                              (point) (point-max))))
          (let* ((file (setq al/ytdl-file-name
                             (buffer-substring-no-properties
                              (point) (pos-eol))))
                 ;; Something probably went wrong if the name is so long.
                 (file (al/shorten-string file 200))
                 (file (replace-regexp-in-string "/" "-" file)))
            (message "-- yt-fn file: %s" file)
            (ytdl--download-async al/ytdl-url
                                  (al/download-dir-file file)
                                  al/ytdl-args)))))))

(defun al/ytdl-setup-buffer (url args)
  "Setup and return buffer for `ytdl-command' process."
  (let* ((id  (al/url-youtube-video-id url))
         (buf (get-buffer-create (concat "*al/ytdl: " id "*"))))
    (with-current-buffer buf
      (erase-buffer)
      (setq al/ytdl-url  url
            al/ytdl-args args))
    buf))

;;;###autoload
(defun al/ytdl-download (url-or-id &optional args)
  "Download youtube video.

URL-OR-ID can be a full video URL or just its ID.

ARGS are additional arguments for `ytdl-command'.  See
`al/ytdl-arg-list' for the meaning of ARGS."
  (interactive
   (list (let ((prompt "Download video from: ")
               (ids (al/url-youtube-video-id-candidates)))
           (if (cdr ids)
               (completing-read prompt ids)
             (read-from-minibuffer prompt (al/url-youtube-video (car ids)))))
         (completing-read "Arguments: "
                          (mapcar #'cdr al/ytdl-args-alist))))
  (let* ((url (or (al/check-url url-or-id)
                  (al/url-youtube-video url-or-id)))
         (args (al/ytdl-arg-list args))
         (buf (al/ytdl-setup-buffer url args)))
    (make-process :name "ytdl-get-file-name"
                  :buffer buf
                  :command `(,ytdl-command
                             "--no-warnings"
                             "--print" "filename"
                             ,@args
                             "--" ,url)
                  :sentinel #'al/ytdl-file-name-sentinel)))

(provide 'al-ytdl)

;;; al-ytdl.el ends here
