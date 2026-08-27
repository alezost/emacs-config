;;; man.el --- Settings for `man' package  -*- lexical-binding: t -*-

(require 'seq)
(require 'man)
(require 'al-places)
(require 'al-key)
(require 'al-file)
(require 'al-visual)

(al/bind-keys
  :map Man-mode-map
  ("M-S-↑" 'Man-previous-section)
  ("M-S-↓" 'Man-next-section)
  ("h" 'Man-previous-section)
  ("n" 'Man-next-section)
  ("m" 'Man-goto-section)
  ("g" 'Man-update-manpage))

(setq
 Man-notify-method 'pushy
 Man-header-file-path
 (append (seq-keep (lambda (p)
                     (al/file-if-exists
                      (expand-file-name "include" p)))
                   (al/guix-profiles))
         Man-header-file-path))

(al/mode-line-default-buffer-identification 'Man-mode)

;;; man.el ends here
