;;; w3m.el --- Settings for `w3m' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'w3m)
(require 'al-places)
(require 'al-key)
(require 'al-w3m)

(al/bind-keys
  :map w3m-mode-map
  "c" "u" "k" "M-s" [left]
  ("<ctrl-m> a"  (emms-add-url (w3m-anchor)))
  ("<ctrl-m> p"  (emms-play-url (w3m-anchor)))
  ("i"         . w3m-toggle-inline-image)
  ("I"         . w3m-toggle-inline-images)
  ("b"         . w3m-bookmark-view)
  ("y"         . w3m-history)
  (","         . w3m-view-previous-page)
  ("p"         . w3m-view-next-page)
  ("h"         . al/w3m-previous-url)
  ("n"         . al/w3m-next-url)
  ("<backtab>" . w3m-previous-form)
  ("<tab>"     . w3m-next-form)
  ("R"         . w3m-redisplay-this-page)
  ("g"         . w3m-reload-this-page)
  ("j"         . w3m-goto-url)
  ("."         . w3m-previous-anchor)
  ("e"         . w3m-next-anchor)
  ("o"         . w3m-view-parent-page)
  ("O"           (w3m-view-parent-page 0))
  ("U"         . w3m-view-this-url-new-session)
  ("u 0"         (browse-url w3m-current-url))
  ("u u"         (browse-url (w3m-anchor)))
  ("u RET"       (browse-url (w3m-anchor)))
  ("c 0"       . w3m-print-current-url)
  ("c RET"     . w3m-print-this-url)
  ("s"         . al/w3m-wget)
  ("w"         . al/w3m-wget)
  ("C-w w"     . al/w3m-wget)
  ("C-w m"       (al/url-wget-mp3 (w3m-anchor)))
  ("C-c C-f"   . w3m-next-buffer)
  ("C-c C-b"   . w3m-previous-buffer))

(al/w3m-bind-number-keys #'al/w3m-switch-to-buffer)
(al/w3m-bind-number-keys #'al/w3m-kill-buffer "k")

(setq
 w3m-add-user-agent nil
 w3m-use-cookies nil
 w3m-confirm-leaving-secure-page nil
 w3m-use-title-buffer-name t    ; don't duplicate title in the mode-line
 w3m-show-graphic-icons-in-mode-line nil
 w3m-modeline-image-status-on "🌼"
 w3m-modeline-status-off ""
 w3m-modeline-separator ""

 al/w3m-search-link-depth 20
 al/w3m-search-re "[^[:alnum:]]*\\<%s\\>")

(al/eval-after-load w3m-save
  (setq
   w3m-save-buffer-html-only t
   w3m-default-save-directory (al/download-dir-file "html")))

(al/eval-after-load w3m-form
  (defconst al/w3m-form-keys
    '(("u" . w3m-form-input-select-set)))
  (al/bind-keys-from-vars 'w3m-form-input-select-keymap
    '(al/lazy-moving-keys al/w3m-form-keys)))

;;; w3m.el ends here
