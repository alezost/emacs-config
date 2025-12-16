;;; mmedia.el --- Using multimedia stuff inside Emacs  -*- lexical-binding: t -*-

;; Copyright © 2014–2025 Alex Kost

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


;;; EMMS

(setq
 emms-directory (al/emacs-data-dir-file "emms")
 emms-playlist-sort-prefix "s")

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
 ("b" . emms-browser)
 ("l" . emms)
 ("r" . emms-streams)
 ("g" . al/emms-seek-to)
 ("y" . al/emms-mpv-sync-playing-time)
 ("S" . emms-cache-save)
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

(with-eval-after-load 'emms
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (require 'emms-info)
  (require 'emms-playlist-mode)
  (require 'emms-mark)
  (require 'emms-streams)
  (require 'emms-playlist-sort)
  (require 'emms-browser)
  (require 'emms-bookmarks)
  (require 'emms-last-played)
  (require 'emms-metaplaylist-mode)
  (require 'emms-i18n)
  (require 'emms-cache)

  (setq
   emms-playlist-buffer-name "*EMMS Playlist*"
   emms-mode-line-format " %s"
   emms-playing-time-display-format " %s"
   emms-playlist-default-major-mode 'emms-playlist-mode
   emms-info-functions '(emms-info-libtag emms-info-cueinfo)
   emms-show-format "%s"
   emms-source-file-default-directory al/music-dir)

  (when (require 'emms-player-mpv nil t)
    (require 'al-emms-mpv nil t)
    (setq emms-player-mpv-parameters '("--no-terminal"))
    (emms-player-set emms-player-mpv 'regex
                     (apply #'emms-player-simple-regexp
                            (cons "oga" emms-player-base-format-list)))
    (push 'emms-player-mpv emms-player-list))

  (emms-cache 1)

  (push 'emms-info-initialize-track emms-track-initialize-functions)
  (al/add-hook-maybe 'emms-player-started-hook
    'emms-last-played-update-current)

  (when (require 'emms-state nil t)
    (emms-state-mode))
  (when (require 'al-emms nil t)
    (setq
     emms-mode-line-mode-line-function #'al/emms-mode-line-song-string
     emms-track-description-function #'al/emms-full-track-description)
    (advice-add 'emms-source-play
      :override #'al/emms-source-add-and-play)
    (advice-add 'emms-playlist-mode-insert-track
      :override #'al/emms-playlist-mode-insert-track)))

(with-eval-after-load 'emms-playlist-mode
  (defconst al/emms-playlist-keys
    '("r"
      ("M-r M-l" . al/org-emms-store-link)
      ("SPC" . emms-pause)
      ("S"   . al/emms-save-playlist)
      ("Q"   . emms-stop)
      ("h"   . emms-previous)
      ("u"   . emms-playlist-mode-play-smart)
      ("j"   . emms-playlist-mode-goto-dired-at-point)
      ("w"   . al/emms-playlist-wget)
      ("C-j" . emms-playlist-mode-insert-newline)
      ("C-k"   (beginning-of-line) (emms-playlist-mode-kill-entire-track))
      ("C-H-M-k" . al/emms-playlist-kill-track-and-file)
      ("C-t" . emms-playlist-mode-kill)
      ("H-u" . emms-playlist-mode-undo)
      ("["   . al/emms-mpv-speed-down)
      ("]"   . al/emms-mpv-speed-up)
      ("DEL" . al/emms-mpv-speed-normal)
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
      ("<kp-2>"    (al/set-sound "3%-"))
      ("<kp-5>"    (al/set-sound "3%+"))
      ("<kp-begin>" (al/set-sound "3%+"))
      ("<up>"      (al/set-sound "3%+"))
      ("<down>"    (al/set-sound "3%-"))
      ("<C-up>"    (al/set-sound "1%+"))
      ("<C-down>"  (al/set-sound "1%-"))
      ("<M-up>"    (al/set-sound "10%+"))
      ("<M-down>"  (al/set-sound "10%-")))
    "Alist of auxiliary keys for `emms-playlist-mode-map'.")
  (al/bind-keys-from-vars 'emms-playlist-mode-map
    '(al/free-moving-keys
      al/free-misc-keys
      al/lazy-moving-keys
      al/emms-playlist-keys)
    t)
  (suppress-keymap emms-playlist-mode-map)
  (al/add-hook-maybe 'emms-playlist-mode-hook
    (list 'hl-line-mode
          ;; `emms-playlist-mode' is not defined properly (with
          ;; `define-derived-mode'), it is just a `defun', so
          ;; `after-change-major-mode-hook' doesn't work and `dim'
          ;; doesn't set `mode-name'.  Because of this, add
          ;; `dim-set-major-name' to the playlist hook.
          'dim-set-major-name
          (lambda () (setq-local page-delimiter "^—"))))

  (al/add-hook-maybe 'kill-emacs-hook 'al/emms-save-playlists))

(with-eval-after-load 'emms-later-do
  (setq emms-later-do-interval 0.1))

(al/autoload "emms-cue" emms-info-cueinfo)
(al/autoload "emms-info-libtag" emms-info-libtag)

(with-eval-after-load 'al-emms-notification
  (setq
   al/emms-notification-artist-format "<big>%s</big>"
   al/emms-notification-title-format "<span foreground=\"yellow\">%s</span>"
   al/emms-notification-year-format "<span foreground=\"#84ebeb\">%s</span>"))

;;; mmedia.el ends here
