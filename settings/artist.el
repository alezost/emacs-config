;;; artist.el --- Settings for `artist' package  -*- lexical-binding: t -*-

(require 'artist)
(require 'al-key)

(defconst al/artist-keys
  '(("C-o" . artist-backward-char)
    ("C-u" . artist-forward-char)
    ("C-." . artist-previous-line)
    ("C-e" . artist-next-line))
  "Alist of auxiliary keys for `artist-mode-map'.")
(al/bind-keys-from-vars 'artist-mode-map 'al/artist-keys)

;;; artist.el ends here
