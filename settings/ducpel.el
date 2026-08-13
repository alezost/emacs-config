;;; ducpel.el --- Settings for `ducpel' package  -*- lexical-binding: t -*-

(require 'ducpel)
(require 'al-places)
(require 'al-key)

(setq ducpel-replay-pause 0.3)
(let ((ducpel-dir (al/emacs-my-packages-dir-file "ducpel")))
  (setq
   ducpel-user-levels-directory
   (file-name-as-directory (expand-file-name "levels" ducpel-dir))
   ducpel-user-saves-directory
   (file-name-as-directory (expand-file-name "temp" ducpel-dir))))

(defvar al/games-map)  ; defined in "init"
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
  ("p" . ducpel-next-level))

;;; ducpel.el ends here
