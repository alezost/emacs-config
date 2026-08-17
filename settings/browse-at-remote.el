;;; browse-at-remote.el --- Settings for `browse-at-remote' package  -*- lexical-binding: t -*-

(require 'browse-at-remote)
(require 'al-browse-at-remote)

(advice-add 'browse-at-remote-get-url
  :around #'al/browse-at-remote-get-url)

;;; browse-at-remote.el ends here
