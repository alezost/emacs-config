;;; mmedia.el --- Using multimedia stuff inside Emacs

;; Copyright © 2014-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'al-key)


;;; EMMS

(setq
 emms-directory (al/emacs-data-dir-file "emms")
 emms-playlist-sort-prefix "s")

(al/bind-keys
 :prefix-map al/emms-map
 :prefix-docstring "Map for EMMS."
 :prefix "C-ь"
 ("SPC" . emms-pause)
 ("M-SPC" . emms-stop)
 ("s" . emms-show)
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
 ("u" . emms-play-url)
 ("e"   (emms-play-file
         (read-file-name "Play file: " al/echo-download-dir))))

(al/bind-keys
 :map al/emms-map
 :prefix-map al/emms-add-map
 :prefix-docstring "Map for adding EMMS entries."
 :prefix "a"
 ("t" . emms-add-directory-tree)
 ("d" . emms-add-directory)
 ("f" . emms-add-file)
 ("l" . emms-add-playlist)
 ("u" . emms-add-url)
 ("e"   (emms-add-file
         (read-file-name "Add file: " al/echo-download-dir))))

(with-eval-after-load 'emms
  (require 'emms-source-file)
  (require 'emms-source-playlist)
  (require 'emms-player-mplayer)
  (require 'emms-info)
  (require 'emms-playlist-mode)
  (require 'emms-mark)
  (require 'emms-streams)
  (require 'emms-playlist-sort)
  (require 'emms-browser)
  (require 'emms-bookmarks)
  (require 'emms-last-played)
  (require 'emms-metaplaylist-mode)
  (require 'emms-stream-info)
  (require 'emms-history)
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

  (setq emms-player-list '(emms-player-mplayer))
  (when (and (executable-find "mpv")
             (require 'al-emms-mpv nil t))
    (al/emms-mpv-add-simple-player)
    (push 'emms-player-mpv emms-player-list))

  ;; Do not add `emms-cache-save' to `kill-emacs-hook'.
  (let ((noninteractive t)) (emms-cache 1))

  (push 'emms-info-initialize-track emms-track-initialize-functions)
  (al/add-hook-maybe 'emms-player-started-hook
    'emms-last-played-update-current)

  (when (require 'emms-state nil t)
    (emms-state-mode))
  (when (require 'al-emms nil t)
    (setq
     emms-mode-line-mode-line-function 'al/emms-mode-line-song-string
     emms-track-description-function 'al/emms-full-track-description)
    (advice-add 'emms-source-play
      :override 'al/emms-source-add-and-play)))

(with-eval-after-load 'emms-playlist-mode
  (defconst al/emms-playlist-keys
    '(("SPC" . emms-pause)
      ("S"   . emms-stop)
      ("u"   . emms-playlist-mode-play-smart)
      ("j"   . emms-playlist-mode-goto-dired-at-point)
      ("C-j" . emms-playlist-mode-insert-newline)
      ("C-k"   (beginning-of-line) (emms-playlist-mode-kill-entire-track))
      ("C-t" . emms-playlist-mode-kill)
      ("H-u" . emms-playlist-mode-undo)
      ("<left>"    (al/emms-seek-backward 10))
      ("<right>"   (al/emms-seek-forward 10))
      ("<C-left>"  (al/emms-seek-backward 3))
      ("<C-right>" (al/emms-seek-forward 3))
      ("<M-left>"  (al/emms-seek-backward 60))
      ("<M-right>" (al/emms-seek-forward 60))
      ("<S-left>"  (al/emms-seek-backward 600))
      ("<S-right>" (al/emms-seek-forward 600))
      ("<up>"      (al/set-sound "3%+"))
      ("<down>"    (al/set-sound "3%-"))
      ("<C-up>"    (al/set-sound "1%+"))
      ("<C-down>"  (al/set-sound "1%-"))
      ("<M-up>"    (al/set-sound "10%+"))
      ("<M-down>"  (al/set-sound "10%-")))
    "Alist of auxiliary keys for `emms-playlist-mode-map'.")
  (al/bind-keys-from-vars 'emms-playlist-mode-map
    '(al/free-misc-keys al/lazy-moving-keys al/emms-playlist-keys)
    t)
  (al/add-hook-maybe 'emms-playlist-mode-hook
    '(al/mode-name hl-line-mode)))

(with-eval-after-load 'emms-streams
  (defconst al/emms-stream-keys
    '(("." . emms-stream-previous-line)
      ("e" . emms-stream-next-line)
      ("u" . emms-stream-play))
    "Alist of auxiliary keys for `emms-stream-mode-map'.")
  (al/bind-keys-from-vars 'emms-stream-mode-map 'al/emms-stream-keys)
  (al/add-hook-maybe 'emms-stream-hook
    '(al/mode-name hl-line-mode)))

(al/autoload "emms-cue" emms-info-cueinfo)
(al/autoload "emms-info-libtag" emms-info-libtag)

(with-eval-after-load 'al-emms
  (setq
   al/emms-notification-artist-format "<big>%s</big>"
   al/emms-notification-title-format "<span foreground=\"yellow\">%s</span>"
   al/emms-notification-year-format "<span foreground=\"#84ebeb\">%s</span>"))

;;; mmedia.el ends here
