;;; games.el --- Various settings for games

;; Copyright © 2014–2017 Alex Kost

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

(require 'al-key)


;;; Global keys

(al/bind-keys
 :map ctl-x-map
 :prefix-map al/games-map
 :prefix-docstring "Map for games."
 :prefix "g"
 ("p"   (practice-words 1000 1))
 ("T" . tetris)
 ("t" . typing-game)
 ("d" . ducpel)
 ("D"   (princ ducpel-moves-history (current-buffer)))
 ("s" . snake))


;;; Misc settings and packages

(with-eval-after-load 'gamegrid
  (setq gamegrid-user-score-file-directory
        (al/emacs-data-dir-file "games")))

(al/autoload "typing-practice" practice-words)
(with-eval-after-load 'typing-practice
  (setq typing-practice-time-threshold 2))

(with-eval-after-load 'ducpel
  (setq
   ducpel-levels-directory
   (expand-file-name "ducpel/levels" quelpa-build-dir)
   ducpel-replay-pause 0.3)
  (let ((ducpel-dir (al/emacs-my-packages-dir-file "ducpel")))
    (setq
     ducpel-user-levels-directory
     (file-name-as-directory (expand-file-name "levels" ducpel-dir))
     ducpel-user-saves-directory
     (file-name-as-directory (expand-file-name "temp" ducpel-dir))))
  (al/bind-keys
   :map ducpel-mode-map
   ("o" . ducpel-move-left)
   ("u" . ducpel-move-right)
   ("." . ducpel-move-up)
   ("e" . ducpel-move-down)
   ("h" . ducpel-previous-man)
   ("n" . ducpel-next-man)
   ("," . ducpel-previous-level)
   ("p" . ducpel-next-level)
   ("H-u" . ducpel-undo)))

(setq
 tetris-width 16
 tetris-height 32
 tetris-buffer-width 40
 tetris-buffer-height 40)

(with-eval-after-load 'tetris
  (setq
   tetris-blank-options
   '(((glyph
       ((:type xpm :ascent center :data "! XPM2
16 16 2 1
+ c #151515
. c #000000
++++++++++++++++
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
+..............+
++++++++++++++++")))
      (t ?\s))
     nil nil))

  (al/bind-keys
   :map tetris-mode-map
   ("o"   . tetris-move-left)
   ("u"   . tetris-move-right)
   ("."   . tetris-rotate-prev)
   ("e"   . tetris-rotate-next)
   ("d"   . tetris-move-bottom)
   ("RET" . tetris-move-bottom)
   ("SPC" . tetris-pause-game)))

(with-eval-after-load 'snake
  (al/bind-keys
   :map snake-mode-map
   ("o"   . snake-move-left)
   ("u"   . snake-move-right)
   ("."   . snake-move-up)
   ("e"   . snake-move-down)
   ("SPC" . snake-pause-game)))

(with-eval-after-load 'mana
  (setq mana-character (rot13 "wbuanguna")))

;;; games.el ends here
