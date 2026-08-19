;;; journal.el --- Settings for `journal' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'journal)
(require 'al-places)

(setq
 org-id-files (directory-files al/journal-dir t
                               journal-file-name-regexp)
 org-id-locations-file (al/emacs-data-dir-file "org-id-locations")
 org-id-track-globally t
 org-agenda-files org-id-files
 journal-current-file (car (last org-id-files))

 journal-directory al/journal-dir
 journal-template-file (al/journal-dir-file "template")

 journal-open-block "┃"
 journal-close-block "┃")

(al/eval-at-hook org-mode-hook
  (and (journal-buffer-p)
       (setq-local sentence-end-double-space nil)))

;;; journal.el ends here
