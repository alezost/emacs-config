;;; browse-url.el --- Settings for `browse-url' package  -*- lexical-binding: t -*-

(require 'al-browse-url)

(setq browse-url-browser-function 'tui/choose-browser)

(advice-add 'browse-url-default-browser
  :override #'al/browse-url-default)

;;; browse-url.el ends here
