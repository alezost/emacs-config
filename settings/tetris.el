;;; tetris.el --- Settings for `tetris' package  -*- lexical-binding: t -*-

(require 'tetris)
(require 'al-key)

(setq
 tetris-width 10
 tetris-height 26
 tetris-buffer-width 30
 tetris-buffer-height 28)

(al/bind-keys
  :map tetris-mode-map
  ("←"   'tetris-move-left)
  ("→"   'tetris-move-right)
  ("↑"   'tetris-rotate-prev)
  ("↓"   'tetris-rotate-next)
  ("d"   'tetris-move-bottom)
  ("RET" 'tetris-move-bottom)
  ("SPC" 'tetris-pause-game))

;;; tetris.el ends here
