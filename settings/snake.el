;;; snake.el --- Settings for `snake' package  -*- lexical-binding: t -*-

(require 'snake)
(require 'al-key)

(al/bind-keys
  :map snake-mode-map
  ("o"   . snake-move-left)
  ("u"   . snake-move-right)
  ("."   . snake-move-up)
  ("e"   . snake-move-down)
  ("SPC" . snake-pause-game))

;;; snake.el ends here
