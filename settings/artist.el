;;; artist.el --- Settings for `artist' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros))

(require 'artist)

(al/bind-keys
  :map artist-mode-map
  ("C-←" 'artist-backward-char)
  ("C-→" 'artist-forward-char)
  ("C-↑" 'artist-previous-line)
  ("C-↓" 'artist-next-line))

;;; artist.el ends here
