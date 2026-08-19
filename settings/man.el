;;; man.el --- Settings for `man' package  -*- lexical-binding: t -*-

(require 'seq)
(require 'man)
(require 'al-places)
(require 'al-key)
(require 'al-file)
(require 'al-visual)

(defconst al/man-keys
  '(("M->" . Man-previous-section)
    ("M-E" . Man-next-section)
    ("h" . Man-previous-section)
    ("n" . Man-next-section)
    ("m" . Man-goto-section)
    ("g" . Man-update-manpage))
  "Alist of auxiliary keys for `Man-mode'.")
(al/bind-keys-from-vars 'Man-mode-map
  '(al/button-keys al/man-keys))

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
