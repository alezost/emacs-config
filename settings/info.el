;;; info.el --- Settings for `info' package  -*- lexical-binding: t -*-

(require 'info)
(require 'al-key)
(require 'al-file)

(al/bind-keys
  :map Info-mode-map
  ("↑" 'Info-prev-reference)
  ("↓" 'Info-next-reference)
  ("c" (Info-copy-current-node-name 0))
  ("←" (Info-up) (goto-char (point-min)))
  ("S-←" 'Info-top-node)
  ("→" 'Info-follow-nearest-node)
  ("↷" 'Info-history-back)
  ("↶" 'Info-history-forward)
  ("y" 'Info-history)
  ("k" 'Info-index-next)
  ("h" 'Info-prev)
  ("n" 'Info-next)
  ("H" 'Info-help))

;; `Info-additional-directory-list' is USELESS as it is appended to
;; `Info-directory-list' (by `Info-find-file' or by `Info-insert-dir'),
;; so the default manuals are searched first, while I want my dirs to be
;; searched first.
(info-initialize)
(setq Info-directory-list
      (append (al/existing-files
               (al/devel-dir-file "guix/doc"))
              Info-directory-list))

;;; info.el ends here
