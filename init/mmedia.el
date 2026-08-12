;;; mmedia.el --- Using multimedia stuff inside Emacs  -*- lexical-binding: t -*-

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

(eval-when-compile
  (require 'al-aux-macros))
(require 'al-places)
(require 'al-general)
(require 'al-key)

(declare-function emms-playlist-simple-uniq "emms")


;;; EMMS

(al/setq-no-warnings
 emms-directory (al/emacs-data-dir-file "emms")
 emms-playlist-sort-prefix "s")

(al/eval-after-load al-emms
  (setq
   emms-mode-line-mode-line-function #'al/emms-mode-line-song-string
   emms-track-description-function #'al/emms-full-track-description
   al/emms-file-name-shorten-alist
   (mapcar (lambda (assoc)
             (cons (directory-file-name (expand-file-name (car assoc)))
                   (cdr assoc)))
           `((,(al/download-dir-file "torrents") . "~t")
             (,al/download-dir . "~d")
             (,(al/math-dir-file "video") . "~math")
             ("~/storage/music" . "~M")
             (,al/music-dir . "~m"))))

  (al/bind-keys
    :map al/emms-switch-playlist-map
    ([ctrl-m] . al/emms-switch-to-playlist-buffer))

  (al/call-at-hook emms-playlist-source-inserted-hook
    al/emms-add-size)
  (al/call-at-hook emms-mpv-before-process-hook
    al/emms-playlist-set-mpv-command)

  (advice-add 'emms-source-play
    :override #'al/emms-source-add-and-play)
  (advice-add 'emms-playlist-mode-insert-track
    :override #'al/emms-playlist-mode-insert-track))

(al/bind-keys
 :prefix-map al/emms-map
 :prefix-docstring "Map for EMMS."
 :prefix [ctrl-m]
 ([ctrl-m] . al/emms-switch-to-playlist-buffer)
 ("SPC" . emms-pause)
 ("M-SPC" . emms-stop)
 ("s" . al/emms-show)
 ("m" . emms-state-toggle-mode-line)
 ("n" . al/emms-notification-mode)
 ("B" . emms-browser)
 ("l"   (al/emms-playlist-select t))
 ("b" . al/emms-playlist-select)
 ("C-b" . al/emms-playlist-select)
 ("r" . emms-streams)
 ("g" . al/emms-seek-to)
 ("y" . al/emms-mpv-sync-playing-time)
 ("S" . al/emms-save-playlists)
 ("u"   (emms-playlist-simple-uniq)))

(al/bind-keys
 :map al/emms-map
 :prefix-map al/emms-play-map
 :prefix-docstring "Map for playing EMMS entries."
 :prefix "p"
 ("t" . emms-play-directory-tree)
 ("d" . emms-play-directory)
 ("f" . emms-play-file)
 ("l" . emms-play-playlist)
 ("u" . emms-play-url))

(al/bind-keys
 :map al/emms-map
 :prefix-map al/emms-add-map
 :prefix-docstring "Map for adding EMMS entries."
 :prefix "a"
 ("t" . emms-add-directory-tree)
 ("d" . emms-add-directory)
 ("f" . emms-add-file)
 ("l" . emms-add-playlist)
 ("u" . emms-add-url))

(al/eval-after-load emms
  (setq
   emms-playlist-buffer-name "*EMMS Playlist*"
   emms-show-format "%s")

  (al/require
    emms-source-file
    emms-source-playlist
    emms-info
    emms-playlist-mode
    emms-mark
    emms-streams
    emms-playlist-sort
    emms-browser
    emms-bookmarks
    emms-last-played
    emms-metaplaylist-mode
    emms-i18n
    emms-mpv
    emms-state
    al-emms))

(al/eval-after-load emms-playlist-mode
  (defconst al/emms-playlist-keys
    '("r" "a"
      ("au"  . al/emms-add-url)
      ("M-r M-l" . al/org-emms-store-link)
      ("M-d" . al/emms-edit-track-property)
      ("C-M-d" . al/emms-edit-mpv-command)
      ("SPC" . emms-pause)
      ("S"   . al/emms-save-playlist)
      ("Q"   . emms-stop)
      ("h"   . emms-previous)
      ("u"   . emms-playlist-mode-play-smart)
      ("j"   . emms-playlist-mode-goto-dired-at-point)
      ("H-j"   (dired emms-directory))
      ("w"   . al/emms-playlist-wget)
      ("C-j" . emms-playlist-mode-insert-newline)
      ("C-k"   (beginning-of-line) (emms-playlist-mode-kill-entire-track))
      ("C-H-M-k" . al/emms-playlist-kill-track-and-file)
      ("C-t" . emms-playlist-mode-kill)
      ("M-." . emms-playlist-mode-shift-track-up)
      ("M-e" . emms-playlist-mode-shift-track-down)
      ("H-u" . emms-playlist-mode-undo)
      ("["   . al/emms-mpv-speed-down)
      ("]"   . al/emms-mpv-speed-up)
      ("DEL" . al/emms-mpv-speed-normal)
      ("<kp-home>"  . al/emms-mpv-speed-down)
      ("<kp-prior>" . al/emms-mpv-speed-up)
      ("<kp-up>"    . al/emms-mpv-speed-normal)
      ("o"   . al/emms-mpv-show-progress)
      ("z"   . al/emms-mpv-switch-volume)
      ("v"   . al/set-sound)
      ("<left>"    (al/emms-seek-backward 10))
      ("<right>"   (al/emms-seek-forward 10))
      ("<C-left>"  (al/emms-seek-backward 3))
      ("<C-right>" (al/emms-seek-forward 3))
      ("<M-left>"  (al/emms-seek-backward 60))
      ("<M-right>" (al/emms-seek-forward 60))
      ("<S-left>"  (al/emms-seek-backward 600))
      ("<S-right>" (al/emms-seek-forward 600))
      ("<kp-end>"  (al/emms-seek-backward 3))
      ("<kp-next>" (al/emms-seek-forward 3))
      ("<kp-1>"    (al/emms-seek-backward 3))
      ("<kp-3>"    (al/emms-seek-forward 3))
      ("<kp-4>"    (al/emms-seek-backward 10))
      ("<kp-6>"    (al/emms-seek-forward 10))
      ("<kp-2>"    (al/set-sound "-3"))
      ("<kp-5>"    (al/set-sound "+3"))
      ("<kp-begin>" (al/set-sound "+3"))
      ("<up>"      (al/set-sound "+3"))
      ("<down>"    (al/set-sound "-3"))
      ("<C-up>"    (al/set-sound "+1"))
      ("<C-down>"  (al/set-sound "-1"))
      ("<M-up>"    (al/set-sound "+10"))
      ("<M-down>"  (al/set-sound "-10")))
    "Alist of auxiliary keys for `emms-playlist-mode-map'.")
  (al/bind-keys-from-vars 'emms-playlist-mode-map
    '(al/free-moving-keys
      al/free-misc-keys
      al/lazy-moving-keys
      al/emms-playlist-keys)
    t)
  (suppress-keymap emms-playlist-mode-map)

  (al/eval-at-hook emms-playlist-mode-hook
    (hl-line-mode)
    ;; `emms-playlist-mode' is not defined properly (with
    ;; `define-derived-mode'), it is just a `defun', so
    ;; `after-change-major-mode-hook' doesn't work and `dim'
    ;; doesn't set `mode-name'.  Because of this, add
    ;; `dim-set-major-name' to the playlist hook.
    (al/funcall 'dim-set-major-name)
    (setq-local page-delimiter "^ *—")))

(al/eval-after-load emms-mode-line
  (setq emms-mode-line-format " %s"))

(al/eval-after-load emms-playing-time
  (setq emms-playing-time-display-format " %s"))

(al/autoload "emms-cue" emms-info-cueinfo)
(al/autoload "emms-info-native" emms-info-native)

(al/eval-after-load emms-info
  (add-hook 'emms-track-initialize-functions
            #'emms-info-initialize-track)
  (setq emms-info-functions '(emms-info-native emms-info-cueinfo)))

(al/eval-after-load emms-source-file
  (setq emms-source-file-default-directory al/music-dir))

(al/eval-after-load emms-last-played
  (add-hook 'emms-player-started-hook
            #'emms-last-played-update-current))

(al/eval-after-load emms-later-do
  (setq emms-later-do-interval 0.1))

(al/eval-after-load emms-mpv
  (setq
   emms-mpv-hidden-buffer-names nil
   emms-mpv-progress-remove-finished nil)
  (remove-hook 'emms-mpv-progress-filters
               'emms-mpv-progress-check-file-type)
  (push 'emms-mpv emms-player-list)
  (al/require al-emms-mpv))

(al/eval-after-load emms-state
  (emms-state-mode))

(al/eval-after-load al-emms-mpv
  (al/call-at-hook (emms-player-seeked-functions
                    emms-player-time-set-functions)
    al/emms-mpv-show-video-progress)
  (push '("client-message" . al/emms-mpv-handle-client-message)
        emms-mpv-event-handlers))

(al/eval-after-load al-emms-notification
  (setq
   al/emms-notification-artist-format "<big>%s</big>"
   al/emms-notification-title-format "<span foreground=\"yellow\">%s</span>"
   al/emms-notification-date-format "<span foreground=\"#84ebeb\">%s</span>"))

;;; mmedia.el ends here
