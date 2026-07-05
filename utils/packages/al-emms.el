;;; al-emms.el --- Additional functionality for EMMS  -*- lexical-binding: t -*-

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

(eval-when-compile (require 'cl-lib))
(require 'seq)
(require 'emms)
(require 'emms-playlist-mode)
(require 'emms-state)
(require 'let-macros)
(require 'al-text)
(require 'al-format)
(require 'al-buffer)
(require 'al-visual)
(require 'al-url)

(defun al/emms-seek-forward (seconds)
  "Seek by SECONDS forward.
Interactively, define SECONDS with a numeric prefix."
  (interactive "p")
  (when emms-player-playing-p
    (emms-player-seek seconds)))

(defun al/emms-seek-backward (seconds)
  "Seek by SECONDS backward.
Interactively, define SECONDS with a numeric prefix."
  (interactive "p")
  (al/emms-seek-forward (- seconds)))

(defun al/emms-seek-to (seconds)
  "Seek the current player to SECONDS.
Interactively, prompt for the number of minutes.
With prefix, prompt for the number of seconds."
  (interactive
   (list (if current-prefix-arg
             (read-number "Seconds to seek to: ")
           (* 60 (read-number "Minutes to seek to: ")))))
  (emms-seek-to seconds))

(defun al/emms-source-add-and-play (source &rest args)
  "Add the tracks of SOURCE to EMMS playlist and play the first one."
  (with-current-emms-playlist
    (goto-char (point-max))
    (let ((first-new-track (point)))
      (apply #'emms-playlist-insert-source source args)
      (emms-playlist-select first-new-track)))
  (emms-stop)
  (emms-start))

(defun al/emms-first ()
  "Start playing the first track in the EMMS playlist."
  (interactive)
  (when emms-player-playing-p
    (emms-stop))
  (emms-playlist-current-select-first)
  (emms-start))

(declare-function al/emms-mpv-playing-radio? "al-emms-mpv" ())
(declare-function al/emms-mpv-show-radio-description "al-emms-mpv" ())
(declare-function al/emms-mpv-show-metadata "al-emms-mpv" ())

;;;###autoload
(defun al/emms-show (&optional arg)
  "Describe the current EMMS track in the minibuffer.
If ARG is specified, show metadata of the track."
  (interactive "P")
  (require 'al-emms-mpv)
  (cond (arg
         (al/emms-mpv-show-metadata))
        ((al/emms-mpv-playing-radio?)
         (al/emms-mpv-show-radio-description))
        (t
         (message (format emms-show-format
                          (emms-track-description
                           (emms-playlist-current-selected-track)))))))


;;; Track description

(defvar al/emms-split-track-regexp
  (rx (group (+? any))
      " - "
      (group (+ any)))
  "Regexp used by `al/emms-split-track-name'.")

(defun al/emms-split-track-name (name)
  "Assuming NAME is \"ARTIST - TITLE\" string, return (ALIST TITLE) list."
  (string-match al/emms-split-track-regexp name)
  (list (match-string 1 name)
        (match-string 2 name)))

(defun al/emms-add-size ()
  "Add `al-size' property to tracks in the current buffer.
This function is intended to be added to
`emms-playlist-source-inserted-hook'."
  (dolist (track (emms-playlist-tracks-in-region
                  (point-min) (point-max)))
    (when (and (eq (emms-track-type track) 'file)
               (not (emms-track-get track 'al-size)))
      (when-let* ((attr (file-attributes (emms-track-name track)))
                  (size (file-attribute-size attr)))
        (emms-track-set track 'al-size size)))))

(defun al/emms-playlist-mode-insert-track (track &optional no-newline)
  "Insert the description of TRACK at point.
This is a substitution for `emms-playlist-mode-insert-track'.  The only
difference is that this function does not add `emms-playlist-track-face'
to the whole track line, so the track description can have custom
fontification."
  (emms-playlist-ensure-playlist-buffer)
  (emms-with-inhibit-read-only-t
   (insert (emms-propertize (emms-track-force-description track)
                            'emms-track track))
   (unless no-newline
     (insert "\n"))))

(defun al/emms-format-artist (artist)
  "Return ARTIST formatted to display in EMMS playlist."
  (and artist
       (al/with-face 'alect-author artist)))

(defun al/emms-format-title (title)
  "Return TITLE formatted to display in EMMS playlist."
  (and title
       (al/with-face 'alect-title title)))

(defun al/emms-format-note (note)
  "Return note formatted to display in EMMS playlist."
  (and note
       (al/with-face 'font-lock-comment-face note)))

(defun al/emms-format-album (album)
  "Return ALBUM formatted to display in EMMS playlist."
  (and album
       (al/with-face 'font-lock-function-name-face
         (al/shorten-string album 30))))

(defun al/emms-format-track-number (track-number)
  "Return TRACK-NUMBER formatted to display in EMMS playlist."
  (and track-number
       (al/with-face 'bold
         (format "%02d" (string-to-number track-number)))))

(defun al/emms-format-playing-time (time &optional face format)
  "Return TIME formatted to display in EMMS playlist."
  (format (or format "%s")
          (if time
              (al/with-face (or face 'alect-time)
                (emms-state-format-time time))
            "")))

(defun al/emms-format-date (date)
  "Return DATE formatted to display in EMMS playlist."
  date)

(defun al/emms-full-track-description (track)
  "Return a full description of TRACK.
Intended to be used for `emms-track-description-function'."
  (let ((name (emms-track-name track)))
    (if (string-match-p page-delimiter name)
        name
      (cl-flet ((etg (key) (emms-track-get track key)))
        (let* ((size   (etg 'al-size))
               (size   (if size
                           (concat (al/format-bytes size 3) " ")
                         ""))
               (artist (al/emms-format-artist       (etg 'info-artist)))
               (title  (al/emms-format-title        (etg 'info-title)))
               (note   (al/emms-format-note         (etg 'al-note)))
               (tnum   (al/emms-format-track-number (etg 'info-tracknumber)))
               (album  (al/emms-format-album        (etg 'info-album)))
               (date   (al/emms-format-date     (or (etg 'info-date)
                                                    (etg 'info-year))))
               (progress (etg 'progress))
               (progress (and progress
                              (al/emms-format-playing-time
                               progress
                               'emms-state-current-playing-time)))
               (time (al/emms-format-playing-time
                      (etg 'info-playing-time)
                      'emms-state-total-playing-time
                      (unless progress "%7s")))
               (time (if progress
                         (concat progress "(" time ")")
                       time))
               (title (cond
                       ((memq (emms-track-type track)
                              '(url streamlist))
                        (concat (and title (concat title " "))
                                (al/emms-simple-track-description track)))
                       (title title)
                       (t (al/emms-simple-track-description track))))
               (desc artist)
               (desc (cond
                      ((and album date)
                       (format "%s [%s – %s]" desc date album))
                      (date  (concat desc " [" date "]"))
                      (album (concat desc " [" album "]"))
                      (t desc)))
               (desc (if tnum
                         (concat desc " " tnum ".")
                       desc))
               (desc (cond
                      ((and note desc)
                       (concat note " " desc " "))
                      (note (concat note " "))
                      (desc (concat desc " ")))))
          (concat size time " " desc title))))))

(defun al/emms-short-track-description (track)
  "Return a short description of TRACK suitable for mode-line."
  (or (emms-track-get track 'info-title)
      (let ((type (emms-track-type track))
            (name (emms-track-name track)))
        (cl-case type
         (file (file-name-nondirectory name))
         (url  (url-file-nondirectory (emms-format-url-track-name name)))
         (t    (al/emms-fallback-track-description track))))))

(defun al/emms-fallback-track-description (track)
  "Return \"TYPE: NAME\" description of TRACK."
  (concat (symbol-name (emms-track-type track))
          ": " (emms-track-name track)))

(defun al/emms-simple-track-description (track)
  "Return TRACK description by its type and name.
This is similar to `emms-track-simple-description' except use
`al/emms-file-track-description' if TRACK type is `file'."
  (cl-case (emms-track-type track)
    (file (al/emms-file-name-description (emms-track-name track)))
    ((url streamlist)
     (emms-format-url-track-name (emms-track-name track)))
    (t    (al/emms-fallback-track-description track))))

(defvar al/emms-file-name-shorten-alist nil
  "Alist of file names and their short descriptions.
This variable is used by `al/emms-file-name-description'.")

(defvar al/emms-file-name-track-number-title-regexp nil)
(defvar al/emms-file-name-artist-title-regexp nil)

(defun al/emms-file-name-init-regexps-maybe ()
  "Set regexp variables if needed."
  (unless al/emms-file-name-track-number-title-regexp
    (let ((id-re  (rx (? " [" (1+ (regex "[^/[]")) "]")))
          (ext-re (rx "." (1+ alnum) string-end)))
      (setq
       al/emms-file-name-track-number-title-regexp
       (rx-to-string `(and "/" (group (1+ digit))
                           (1+ (regex "[ .-]"))
                           (group (+? (not ?/)))
                           (regex ,id-re)
                           (regex ,ext-re))
                     'no-group)
       al/emms-file-name-artist-title-regexp
       (rx-to-string `(and "/" (group (+ (not ?/)))
                           " - "
                           (group (+? (not ?/)))
                           (regex ,id-re)
                           (regex ,ext-re))
                     'no-group)))))

(defun al/emms-file-name-description (file-name)
  "Return track description by its FILE-NAME."
  (al/emms-file-name-init-regexps-maybe)
  (let ((alist al/emms-file-name-shorten-alist)
        (res nil))
    (while (and alist (null res))
      (let ((assoc (car alist)))
        (if (string-match (regexp-quote (car assoc))
                          file-name)
            (setq res
                  (concat (substring file-name 0 (match-beginning 0))
                          (al/with-face 'font-lock-function-name-face
                            (cdr assoc))
                          (substring file-name (match-end 0))))
          (setq alist (cdr alist)))))
    (let ((file (or res file-name)))
      (cond
       ((string-match al/emms-file-name-track-number-title-regexp file)
        (concat (substring file 0 (match-beginning 1))
                (al/emms-format-track-number (match-string 1 file))
                (substring file (match-end 1) (match-beginning 2))
                (al/emms-format-title (match-string 2 file))
                (substring file (match-end 2))))
       ;; ((string-match al/emms-file-name-artist-title-regexp file)
       ;;  (concat (substring file 0 (match-beginning 1))
       ;;          (al/emms-format-artist (match-string 1 file))
       ;;          (substring file (match-end 1) (match-beginning 2))
       ;;          (al/emms-format-title (match-string 2 file))
       ;;          (substring file (match-end 2))))
       (t file)))))

(defun al/emms-track-properties (track)
  "Return list of all TRACK properties (fields)."
  (mapcar #'car (cdr track)))

(defun al/emms-edit-track-property ()
  "Modify some property of the track at point.
Edit \\+`al-note' property by default.  With prefix, select one of the
available properties."
  (interactive)
  (let* ((track (emms-playlist-track-at))
         (prop (if current-prefix-arg
                   (intern
                    (completing-read "Edit track property: "
                                     (al/emms-track-properties track)))
                 'al-note))
         (prompt (format "Edit `%s' of \"%s\" track: "
                         prop (emms-track-name track)))
         (value (emms-track-get track prop))
         (string-or-nil? (or (null value) (stringp value)))
         (value (read-string prompt
                             (if string-or-nil?
                                 value
                               (format "%S" value))))
         (value (cond
                 ((string-empty-p value) nil)
                 (string-or-nil? value)
                 (t (read value)))))
    (emms-track-set track prop value)
    (emms-playlist-mode-update-track-function)))


;;; Mode line

(require 'emms-mode-line)

(defvar al/emms-mode-line-song-function
  'al/emms-short-track-description
  "Default function used in `al/emms-mode-line-song-string'.")

(defun al/emms-mode-line-song-string ()
  "Format the currently playing song.
Intended to be used for `emms-mode-line-mode-line-function'."
  (if-let* ((track (emms-playlist-current-selected-track)))
      (format emms-mode-line-format
              (funcall al/emms-mode-line-song-function track))
    " (no track)"))


;;; Playlists

(defvar al/emms-playlist-alias-alist
  '(("m"  . "EMMS-main")
    ("b"  . "EMMS-background")
    ("b2" . "EMMS-background2"))
  "Alist of aliases and full playlist names.")

(defvar al/emms-playlist-mpv-command-alist
  '(("music" "mpv" "--profile=audio")
    ("url"   "mpv" "--profile=url")
    (t       "mpv" "--profile=video"))
  "Alist of playlists and respecting mpv commands.
Each element should have (NAME . CMD) form where:

  NAME is a string matching playlist name (see `al/emms-get-playlist-name'),
  NAME of the last alist element can also be `t' to use CMD for the rest
  playlists;

  CMD is a value for `emms-mpv-command' variable.")

(al/defun-lazy al/emms-all-playlists
  "Return names of all EMMS playlists."
  (mapcar #'file-name-base
          (directory-files emms-directory nil "EMMS-.+.\pl")))

(defun al/emms-get-playlist-name (string)
  "Return EMMS playlist name matching STRING.
STRING can be a full playlist name, its alias from
`al/emms-playlist-alias-alist', or regexp matching playlist name."
  (or (alist-get string al/emms-playlist-alias-alist
                 nil nil #'string=)
      (seq-find (lambda (name)
                  (let ((case-fold-search t))
                    (if (string-match-p "emms-" string)
                        (string-match-p string name)
                      (string-match-p string name 5))))
                (al/emms-all-playlists))
      (error "Cannot define playlist by %S" string)))

(defun al/emms-get-playlist (string)
  "Return EMMS playlist buffer matching STRING.
Open this playlist if is not opened yet.
See `al/emms-get-playlist-name' for details."
  (let ((name (al/emms-get-playlist-name string)))
    (or (get-buffer name)
        (let ((buf  (emms-playlist-new name))
              (file (expand-file-name (concat name ".pl")
                                      emms-directory)))
          (if (file-exists-p file)
              (with-current-buffer buf
                (al/emms-add-source 'emms-source-playlist file)
                buf)
            ;; Actually, this error should never happen: if NAME is
            ;; found then FILE should exist.
            (error "File <%s> does not exist" file))))))

(defvar emms-mpv-command)

(defun al/emms-playlist-set-mpv-command ()
  "Set `emms-mpv-command' for the current playlist.
See `al/emms-playlist-mpv-command-alist' for details."
  (when-letn ((buf-name (buffer-name emms-playlist-buffer))
              (assoc (seq-find
                      (lambda (elt)
                        (or (eq t (car elt))
                            (let ((name (al/emms-get-playlist-name
                                         (car elt))))
                              (string-equal name buf-name))))
                      al/emms-playlist-mpv-command-alist)))
    (with-current-buffer emms-playlist-buffer
      (setq-local emms-mpv-command
                  (append (cdr assoc)
                          '("--keep-open=always"
                            "--msg-color=no"))))))

(defun al/emms-add-source (source &rest args)
  "Add SOURCE tracks to the current (playlist) buffer."
  ;; Originates from `emms-source-add'.
  (save-excursion
    (goto-char (point-max))
    (apply #'emms-playlist-insert-source source args))
  (when (or (not emms-playlist-selected-marker)
	    (not (marker-position emms-playlist-selected-marker)))
    (emms-playlist-select-first)))

(defun al/emms-add-url (url)
  "Add URL to the current (playlist) buffer."
  (interactive
   (let* ((prompt "Add URL: ")
          (candidates (al/url-candidates))
          (url (if (cdr candidates)
                   (completing-read prompt candidates)
                 (read-string prompt (car candidates)))))
     (list url)))
  (al/emms-add-source 'emms-source-url url))

(defun al/emms-add-source-to-playlist (name source &rest args)
  "Add SOURCE tracks to playlist NAME.
See `al/emms-get-playlist' for the meaning of NAME string."
  (with-current-buffer (al/emms-get-playlist name)
    (apply #'al/emms-add-source source args)))

(defun al/emms-add-file-to-playlist (name file)
  "Add FILE to playlist NAME.
See `al/emms-get-playlist' for the meaning of NAME string."
  (al/emms-add-source-to-playlist
   name 'emms-source-file (substring-no-properties file)))

(defvar al/emms-switch-playlist-map (make-sparse-keymap))

(defun al/emms-playlist-buffers ()
  "Return a list of EMMS playlist buffers.
This is similar to `emms-playlist-buffer-list' except it does not check
`buffer-list' for new playlists."
  (setq emms-playlist-buffers
        (seq-filter #'buffer-live-p
		    emms-playlist-buffers)))

(declare-function al/emms-mpv-raise-frame "al-emms-mpv")

;;;###autoload
(defun al/emms-playlist-play (string)
  "Switch to EMMS playlist buffer matching STRING and start/resume playing.
Interactively, prompt for an existing playlist."
  (interactive
   (list (completing-read "Switch to buffer: " (al/emms-all-playlists))))
  (al/display-buffer (al/emms-get-playlist string))
  (when emms-player-playing-p
    (al/emms-mpv-raise-frame))
  (emms-start)
  (when-let ((resume (emms-player-get emms-player-playing-p 'resume)))
    (funcall resume)))

;;;###autoload
(defun al/emms-playlist-select (&optional arg)
  "Prompt for EMMS playlist buffer and switch to it.
If ARG is nil, prompt for a buffer that is already opened.
Otherwise (interactively, with prefix), prompt for any existing
playlist."
  (interactive "P")
  (let* ((names (if arg
                    (al/emms-all-playlists)
                  (mapcar #'buffer-name (al/emms-playlist-buffers))))
         (name (completing-read "Switch to buffer: " names)))
    (al/display-buffer (al/emms-get-playlist name))))

;;;###autoload
(defun al/emms-switch-to-playlist-buffer (&optional arg)
  "Switch to the next EMMS playlist.
If ARG is non-nil, prompt for the playlist."
  (interactive "P")
  (let ((buffers (al/emms-playlist-buffers)))
    (if (or arg
            emms-playlist-buffer-p
            (null buffers))
        (al/rotate-or-select-buffer
         buffers
         "There are no EMMS playlists."
         (when arg "EMMS buffer: "))
      (al/display-buffer emms-playlist-buffer)))
  (set-transient-map al/emms-switch-playlist-map))

(declare-function wget "wget" t)

(defun al/emms-playlist-wget ()
  "Run `wget' on the URL track at point."
  (interactive)
  (let* ((track (emms-playlist-track-at))
         (type  (emms-track-get track 'type))
         (url   (emms-track-get track 'name)))
    (unless (eq type 'url)
      (user-error "Current track is not of 'url' type."))
    (wget url)))

(defun al/emms-playlist-kill-track-and-file (&optional no-confirm)
  "Kill track at point and delete its file.
If NO-CONFIRM is non-nil, delete the file without confirmation."
  (interactive)
  (if-let1 ((track (emms-playlist-track-at))
            (type (emms-track-get track 'type))
            (name (emms-track-get track 'name)))
      (progn
        (when (and (eq type 'file)
                   (or no-confirm
                       (y-or-n-p (format "Delete %S?" name))))
          (message "Deleting file: %S." name)
          (delete-file name))
        (emms-playlist-mode-kill-entire-track))
    (user-error "No track at point")))

(defvar emms-source-playlist-ask-before-overwrite)

;;;###autoload
(defun al/emms-save-playlist ()
  "Save the current EMMS playlist."
  (interactive)
  (when emms-playlist-buffer-p
    (let ((emms-source-playlist-ask-before-overwrite nil)
          (emms-playlist-buffer (current-buffer)))
      (emms-playlist-save
       'native
       (expand-file-name (concat (buffer-name) ".pl")
                         emms-directory)))))

(declare-function emms-mpv-save-current-progress-maybe "emms-mpv")

;;;###autoload
(defun al/emms-save-playlists ()
  "Save all EMMS playlists."
  (interactive)
  (emms-mpv-save-current-progress-maybe)
  (dolist (buf emms-playlist-buffers)
    (when (buffer-live-p buf)
      (with-current-buffer buf
        (al/emms-save-playlist)))))

;;;###autoload
(defun al/emms-update-all-tracks ()
  "Update all tracks in the current playlist."
  (interactive)
  (emms-playlist-ensure-playlist-buffer)
  (goto-char (point-min))
  (emms-walk-tracks
    (emms-playlist-mode-update-track-function)))

(provide 'al-emms)

;;; al-emms.el ends here
