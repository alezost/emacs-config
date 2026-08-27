;;; artist.el --- Settings for `artist' package  -*- lexical-binding: t -*-

(require 'artist)
(require 'al-key)

(al/bind-keys
  :map artist-mode-map
  ("C-←" 'artist-backward-char)
  ("C-→" 'artist-forward-char)
  ("C-↑" 'artist-previous-line)
  ("C-↓" 'artist-next-line))

;;; artist.el ends here
