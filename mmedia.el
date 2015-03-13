;;; mmedia.el --- Using multimedia stuff inside Emacs

;; Copyright © 2014-2015 Alex Kost

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


;;; EMMS

(use-package emms
  :defer t
  :pre-load
  (setq
   emms-directory (al/emacs-data-dir-file "emms")
   emms-playlist-sort-prefix "s")
  :init
  (bind-keys
   :prefix-map al/emms-map
   :prefix-docstring "Map for EMMS."
   :prefix "C-ь"
   ("SPC" . emms-pause)
   ("M-SPC" . emms-stop)
   ("s" . emms-show)
   ("n" . utl-emms-notification-mode)
   ("b" . emms-browser)
   ("l" . emms)
   ("r" . emms-streams)
   ("g" . utl-emms-seek-to)
   ("y" . emms-cache-sync)
   ("S" . emms-cache-save)
   ("u" . (lambda () (interactive) (emms-playlist-simple-uniq))))

  (bind-keys
   :map al/emms-map
   :prefix-map al/emms-play-map
   :prefix-docstring "Map for playing EMMS entries."
   :prefix "p"
   ("t" . emms-play-directory-tree)
   ("d" . emms-play-directory)
   ("f" . emms-play-file)
   ("l" . emms-play-playlist)
   ("u" . emms-play-url)
   ("e" . (lambda () (interactive)
            (emms-play-file
             (read-file-name "Play file: " al/echo-download-dir)))))

  (bind-keys
   :map al/emms-map
   :prefix-map al/emms-add-map
   :prefix-docstring "Map for adding EMMS entries."
   :prefix "a"
   ("t" . emms-add-directory-tree)
   ("d" . emms-add-directory)
   ("f" . emms-add-file)
   ("l" . emms-add-playlist)
   ("u" . emms-add-url)
   ("e" . (lambda () (interactive)
            (emms-add-file
             (read-file-name "Add file: " al/echo-download-dir)))))

  :config
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
             (require 'emms-player-mpv nil t))
    (push 'emms-player-mpv emms-player-list))

  ;; Do not add `emms-cache-save' to `kill-emacs-hook'.
  (let ((noninteractive t)) (emms-cache 1))

  (push 'emms-info-initialize-track emms-track-initialize-functions)
  (al/add-hook-maybe 'emms-player-started-hook
    'emms-last-played-update-current)

  (when (require 'utl-emms nil t)
    (setq
     emms-mode-line-mode-line-function 'utl-emms-mode-line-song-string
     emms-track-description-function 'utl-emms-full-track-description)
    (utl-emms-mode-line 1)
    (defalias 'emms-source-play 'utl-emms-source-add-and-play)))

(use-package emms-playlist-mode
  :defer t
  :config
  (defconst al/emms-playlist-keys
    '(("SPC" . emms-pause)
      ("S"   . emms-stop)
      ("u"   . emms-playlist-mode-play-smart)
      ("j"   . emms-playlist-mode-goto-dired-at-point)
      ("C-j" . emms-playlist-mode-insert-newline)
      ("C-k"   (beginning-of-line) (emms-playlist-mode-kill-entire-track))
      ("C-t" . emms-playlist-mode-kill)
      ("H-u" . emms-playlist-mode-undo))
    "Alist of auxiliary keys for `emms-playlist-mode-map'.")
  (al/bind-keys-from-vars 'emms-playlist-mode-map
    '(al/lazy-moving-keys al/emms-playlist-keys)
    t)
  (al/add-hook-maybe 'emms-playlist-mode-hook
    '(utl-mode-name hl-line-mode)))

(use-package emms-streams
  :defer t
  :config
  (defconst al/emms-stream-keys
    '(("." . emms-stream-previous-line)
      ("e" . emms-stream-next-line)
      ("u" . emms-stream-play))
    "Alist of auxiliary keys for `emms-stream-mode-map'.")
  (al/bind-keys-from-vars 'emms-stream-mode-map 'al/emms-stream-keys)
  (al/add-hook-maybe 'emms-stream-hook
    '(utl-mode-name hl-line-mode)))

(use-package emms-cue
  :defer t
  :commands emms-info-cueinfo)

(use-package emms-info-libtag
  :defer t
  :commands emms-info-libtag)

(use-package utl-emms
  :defer t
  :diminish (utl-emms-notification-mode . " 🎧")
  :config
  (setq
   utl-emms-notification-artist-format "<big>%s</big>"
   utl-emms-notification-title-format "<span foreground=\"yellow\">%s</span>"
   utl-emms-notification-year-format "<span foreground=\"#84ebeb\">%s</span>"))

;;; mmedia.el ends here
