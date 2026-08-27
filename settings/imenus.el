;;; imenus.el --- Settings for `imenus' package  -*- lexical-binding: t -*-

(require 'imenus)
(require 'al-places)
(require 'al-key)
(require 'al-file)
(require 'al-imenus)

(al/bind-keys
  :map imenus-minibuffer-map
  ("C-r" 'imenus-rescan)
  ("C-s" 'imenus-exit-to-isearch)
  ("M-s" 'imenus-exit-to-occur))

(setq
 imenus-delimiter imenu-level-separator
 al/imenus-elisp-directories
 (append (list al/emacs-init-dir
               al/emacs-settings-dir
               al/emacs-my-packages-dir)
         (al/subdirs al/emacs-utils-dir)))

;;; imenus.el ends here
