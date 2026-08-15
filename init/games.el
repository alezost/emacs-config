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

;;; Code:

(eval-when-compile
  (require 'al-aux-macros))
(require 'al-general)
(require 'al-places)
(require 'al-key)


;;; Global keys

(al/bind-keys
 :map ctl-x-map
 :prefix-map al/games-map
 :prefix-doc "Map for games."
 :prefix-key "g"
 ("T" . tetris)
 ("d" . ducpel)
 ("s" . snake))


;;; Misc settings and packages

(al/eval-after-load gamegrid
  (setq gamegrid-user-score-file-directory
        (al/emacs-data-dir-file "games")))

(al/eval-after-load ducpel
  (al/load-settings "ducpel"))

(al/eval-after-load tetris
  (al/load-settings "tetris"))

(al/eval-after-load snake
  (al/load-settings "snake"))

;;; games.el ends here
