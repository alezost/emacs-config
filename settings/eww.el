;;; eww.el --- Settings for `eww' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-key-macros)
  (require 'al-aux-macros))

(require 'eww)

;; Functions to get link at point: `eww-links-at-point',
;; `shr-url-at-point'; more complicated: `eww-suggested-uris'.

(al/bind-keys
  :map eww-mode-map
  ("<ctrl-m> a" (emms-add-url (shr-url-at-point nil)))
  ("<ctrl-m> p" (emms-play-url (shr-url-at-point nil)))
  ("i"      'eww-toggle-images)
  ("I"      'eww-toggle-images)
  ("y"      'eww-list-histories)
  ("↶"      'eww-back-url)
  ("↷"      'eww-forward-url)
  ("h"      'eww-previous-url)
  ("n"      'eww-next-url)
  ("<tab>"  'shr-next-link)
  ("↑"      'shr-previous-link)
  ("↓"      'shr-next-link)
  ("0"      (browse-url (eww-current-url)))
  ("l"      'eww-buffer-list))

(setq shr-inhibit-images t)

(al/eval-at-hook eww-mode-hook
  ;; `eww-mode' sets `browse-url-browser-function' to `eww-browse-url'.
  (kill-local-variable 'browse-url-browser-function))

;;; eww.el ends here
