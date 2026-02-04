;;; games.el --- Various settings for games  -*- lexical-binding: t -*-

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


;;; Global keys

(al/bind-keys
 :map ctl-x-map
 :prefix-map al/games-map
 :prefix-docstring "Map for games."
 :prefix "g"
 ("T" . tetris)
 ("d" . ducpel)
 ("s" . snake))


;;; Misc settings and packages

(al/with-eval-after-load gamegrid
  (setq gamegrid-user-score-file-directory
        (al/emacs-data-dir-file "games")))

(al/with-eval-after-load ducpel
  (setq ducpel-replay-pause 0.3)
  (let ((ducpel-dir (al/emacs-my-packages-dir-file "ducpel")))
    (setq
     ducpel-user-levels-directory
     (file-name-as-directory (expand-file-name "levels" ducpel-dir))
     ducpel-user-saves-directory
     (file-name-as-directory (expand-file-name "temp" ducpel-dir))))

  (al/bind-keys
    :map al/games-map
    ("D" (princ ducpel-moves-history (current-buffer))))

  (al/bind-keys
    :map ducpel-mode-map
    ("o" . ducpel-move-left)
    ("u" . ducpel-move-right)
    ("." . ducpel-move-up)
    ("e" . ducpel-move-down)
    ("h" . ducpel-previous-man)
    ("n" . ducpel-next-man)
    ("," . ducpel-previous-level)
    ("p" . ducpel-next-level)))

(al/with-eval-after-load tetris
  (setq
   tetris-width 10
   tetris-height 26
   tetris-buffer-width 30
   tetris-buffer-height 28)

  (al/bind-keys
   :map tetris-mode-map
   ("o"   . tetris-move-left)
   ("u"   . tetris-move-right)
   ("."   . tetris-rotate-prev)
   ("e"   . tetris-rotate-next)
   ("d"   . tetris-move-bottom)
   ("RET" . tetris-move-bottom)
   ("SPC" . tetris-pause-game)))

(al/with-eval-after-load snake
  (al/bind-keys
   :map snake-mode-map
   ("o"   . snake-move-left)
   ("u"   . snake-move-right)
   ("."   . snake-move-up)
   ("e"   . snake-move-down)
   ("SPC" . snake-pause-game)))

;;; games.el ends here
