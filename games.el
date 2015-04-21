;;; games.el --- Various settings for games

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


;;; Global keys

(bind-keys*
 :prefix-map al/games-map
 :prefix-docstring "Map for games."
 :prefix "M-G"
 ("p" . (lambda () (interactive) (practice-words 1000 1)))
 ("t" . tetris)
 ("d" . ducpel)
 ("D" . (lambda () (interactive)
          (princ ducpel-moves-history (current-buffer))))
 ("s" . snake))


;;; Misc settings and packages

(use-package gamegrid
  :defer t
  :config
  (setq gamegrid-user-score-file-directory
        (al/emacs-data-dir-file "games")))

(use-package typing-practice
  :defer t
  :commands practice-words
  :config
  (setq typing-practice-time-threshold 2))

(use-package ducpel
  :defer t
  :config
  (setq
   ducpel-levels-directory
   (expand-file-name "ducpel/levels" quelpa-build-dir)
   ducpel-replay-pause 0.3)
  (let ((ducpel-dir (al/emacs-dir-file "ducpel")))
    (setq
     ducpel-user-levels-directory
     (file-name-as-directory (expand-file-name "levels" ducpel-dir))
     ducpel-user-saves-directory
     (file-name-as-directory (expand-file-name "temp" ducpel-dir))))
  (bind-keys
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

(use-package tetris
  :defer t
  :init
  (setq
   tetris-width 16
   tetris-height 32
   tetris-buffer-width 40
   tetris-buffer-height 40)
  :config
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

  (bind-keys
   :map tetris-mode-map
   ("o"   . tetris-move-left)
   ("u"   . tetris-move-right)
   ("."   . tetris-rotate-prev)
   ("e"   . tetris-rotate-next)
   ("d"   . tetris-move-bottom)
   ("RET" . tetris-move-bottom)
   ("SPC" . tetris-pause-game)))

(use-package snake
  :defer t
  :config
  (bind-keys
   :map snake-mode-map
   ("o"   . snake-move-left)
   ("u"   . snake-move-right)
   ("."   . snake-move-up)
   ("e"   . snake-move-down)
   ("SPC" . snake-pause-game)))

(use-package mana
  :defer t
  :init (al/add-my-package-to-load-path-maybe "mana")
  :config
  (setq mana-character (rot13 "wbuanguna")))

;;; games.el ends here
