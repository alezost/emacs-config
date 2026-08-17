;;; imenus.el --- Settings for `imenus' package  -*- lexical-binding: t -*-

(require 'imenus)
(require 'al-places)
(require 'al-key)
(require 'al-file)
(require 'al-imenus)

(defconst al/imenus-keys
  '(("C-r" . imenus-rescan)
    ("C-s" . imenus-exit-to-isearch)
    ("M-s" . imenus-exit-to-occur)))

(al/bind-keys-from-vars 'imenus-minibuffer-map 'al/imenus-keys)

(setq
 imenus-delimiter imenu-level-separator
 al/imenus-elisp-directories
 (append (list al/emacs-init-dir
               al/emacs-settings-dir
               al/emacs-my-packages-dir)
         (al/subdirs al/emacs-utils-dir)))

;;; imenus.el ends here
