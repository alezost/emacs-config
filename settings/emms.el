;;; emms.el --- Settings for `emms' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(eval-and-compile
  (al/require
    emms
    emms-source-file
    emms-source-playlist
    emms-info
    emms-mode-line
    emms-playing-time
    emms-playlist-mode
    emms-playlist-sort
    emms-mark
    emms-streams
    emms-last-played
    emms-i18n
    emms-mpv
    emms-state
    al-emms
    al-emms-mpv
    al-places
    al-general
    al-key))

(al/autoload "emms-cue" emms-info-cueinfo)
(al/autoload "emms-info-native" emms-info-native)

(al/bind-keys
  :map al/emms-switch-playlist-map
  ([ctrl-m] 'al/emms-switch-to-playlist-buffer))

(al/bind-keys
  :map emms-playlist-mode-map
  "r" "a"
  ("au"         'al/emms-add-url)
  ("M-r M-l"    'al/org-emms-store-link)
  ("M-d"        'al/emms-edit-track-property)
  ("C-M-d"      'al/emms-edit-mpv-command)
  ("SPC"        'emms-pause)
  ("S"          'al/emms-save-playlist)
  ("Q"          'emms-stop)
  ("h"          'emms-previous)
  ("↑"          'previous-line)
  ("↓"          'next-line)
  ("→"          'emms-playlist-mode-play-smart)
  ("j"          'emms-playlist-mode-goto-dired-at-point)
  ("H-j"        (dired emms-directory))
  ("w"          'al/emms-playlist-wget)
  ("C-j"        'emms-playlist-mode-insert-newline)
  ("C-k"        (beginning-of-line) (emms-playlist-mode-kill-entire-track))
  ("C-H-M-k"    'al/emms-playlist-kill-track-and-file)
  ("C-t"        'emms-playlist-mode-kill)
  ("M-↑"        'emms-playlist-mode-shift-track-up)
  ("M-↓"        'emms-playlist-mode-shift-track-down)
  ("H-u"        'emms-playlist-mode-undo)
  ("["          'al/emms-mpv-speed-down)
  ("]"          'al/emms-mpv-speed-up)
  ("DEL"        'al/emms-mpv-speed-normal)
  ("<kp-home>"  'al/emms-mpv-speed-down)
  ("<kp-prior>" 'al/emms-mpv-speed-up)
  ("<kp-up>"    'al/emms-mpv-speed-normal)
  ("o"          'al/emms-mpv-show-progress)
  ("z"          'al/emms-mpv-switch-volume)
  ("v"          'al/set-sound)
  ("<left>"     (al/emms-seek-backward 10))
  ("<right>"    (al/emms-seek-forward 10))
  ("<C-left>"   (al/emms-seek-backward 3))
  ("<C-right>"  (al/emms-seek-forward 3))
  ("<M-left>"   (al/emms-seek-backward 60))
  ("<M-right>"  (al/emms-seek-forward 60))
  ("<S-left>"   (al/emms-seek-backward 600))
  ("<S-right>"  (al/emms-seek-forward 600))
  ("<kp-end>"   (al/emms-seek-backward 3))
  ("<kp-next>"  (al/emms-seek-forward 3))
  ("<kp-1>"     (al/emms-seek-backward 3))
  ("<kp-3>"     (al/emms-seek-forward 3))
  ("<kp-4>"     (al/emms-seek-backward 10))
  ("<kp-6>"     (al/emms-seek-forward 10))
  ("<kp-2>"     (al/set-sound "-3"))
  ("<kp-5>"     (al/set-sound "+3"))
  ("<kp-begin>" (al/set-sound "+3"))
  ("<up>"       (al/set-sound "+3"))
  ("<down>"     (al/set-sound "-3"))
  ("<C-up>"     (al/set-sound "+1"))
  ("<C-down>"   (al/set-sound "-1"))
  ("<M-up>"     (al/set-sound "+10"))
  ("<M-down>"   (al/set-sound "-10")))

(suppress-keymap emms-playlist-mode-map)

(setq
 emms-playlist-buffer-name "*EMMS Playlist*"
 emms-show-format "%s"
 emms-mode-line-format " %s"
 emms-playing-time-display-format " %s"
 emms-info-functions '(emms-info-native emms-info-cueinfo)
 emms-source-file-default-directory al/music-dir
 emms-later-do-interval 0.1
 emms-player-list '(emms-mpv)
 emms-mode-line-mode-line-function #'al/emms-mode-line-song-string
 emms-track-description-function #'al/emms-full-track-description

 emms-mpv-hidden-buffer-names nil
 emms-mpv-progress-remove-finished nil

 al/emms-file-name-shorten-alist
 (mapcar (lambda (assoc)
           (cons (directory-file-name (expand-file-name (car assoc)))
                 (cdr assoc)))
         `((,(al/download-dir-file "torrents") . "~t")
           (,al/download-dir . "~d")
           (,(al/math-dir-file "video") . "~math")
           ("~/storage/music" . "~M")
           (,al/music-dir . "~m"))))

(push '("client-message" . al/emms-mpv-handle-client-message)
      emms-mpv-event-handlers)

(add-hook 'emms-track-initialize-functions
          #'emms-info-initialize-track)
(add-hook 'emms-player-started-hook
          #'emms-last-played-update-current)
(add-hook 'emms-playlist-source-inserted-hook
          #'al/emms-add-size)
(add-hook 'emms-mpv-before-process-hook
          #'al/emms-playlist-set-mpv-command)
(remove-hook 'emms-mpv-progress-filters
             #'emms-mpv-progress-check-file-type)

(al/call-at-hook (emms-player-seeked-functions
                  emms-player-time-set-functions)
  al/emms-mpv-show-video-progress)

(al/eval-at-hook emms-playlist-mode-hook
  (hl-line-mode)
  ;; `emms-playlist-mode' is not defined properly (with
  ;; `define-derived-mode'), it is just a `defun', so
  ;; `after-change-major-mode-hook' doesn't work and `dim'
  ;; doesn't set `mode-name'.  Because of this, add
  ;; `dim-set-major-name' to the playlist hook.
  (al/funcall 'dim-set-major-name)
  (setq-local page-delimiter "^ *—"))

(advice-add 'emms-source-play
  :override #'al/emms-source-add-and-play)
(advice-add 'emms-playlist-mode-insert-track
  :override #'al/emms-playlist-mode-insert-track)

(emms-state-mode)

(al/eval-after-load al-emms-notification
  (setq
   al/emms-notification-artist-format "<big>%s</big>"
   al/emms-notification-title-format "<span foreground=\"yellow\">%s</span>"
   al/emms-notification-date-format "<span foreground=\"#84ebeb\">%s</span>"))

;;; mmedia.el ends here
