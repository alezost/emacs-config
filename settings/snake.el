;;; snake.el --- Settings for `snake' package  -*- lexical-binding: t -*-

(require 'snake)
(require 'al-key)

(al/bind-keys
  :map snake-mode-map
  ("←"   'snake-move-left)
  ("→"   'snake-move-right)
  ("↑"   'snake-move-up)
  ("↓"   'snake-move-down)
  ("SPC" 'snake-pause-game))

(setq snake-tick-period 0.1)  ; speed

;;; snake.el ends here
