;;; dired.el --- Settings for `dired' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'dired)
(require 'dired-x)
(require 'al-dired)
(require 'al-key)
(require 'al-complete)
(require 'al-visual)

(al/bind-keys
  :map dired-mode-map
  "c" "M-$"
  ("SPC"   'al/dired-get-size)
  ("N"     'dired-create-directory)
  ("M"     'al/dired-man-or-chmod)
  ("f"     'dired-show-file-type)
  ("F"     'al/dired-stat)
  ("^"     'al/dired-append-marked-files)
  ("←"     'dired-up-directory)
  ("→"     'al/dired-find-file)
  ("S-→"  'dired-do-find-marked-files)
  ("↑"     'dired-previous-line)
  ("↓"     'dired-next-line)
  ("S-↑"   'dired-prev-dirline)
  ("S-↓"   'dired-next-dirline)
  ("H-a"   'al/dired-beginning-of-buffer)
  ("H-i"   'al/dired-end-of-buffer)
  ("M-d"   'dired-toggle-read-only)
  ("p"     'pathify-dired)
  ("t"     'image-dired-display-thumbs)
  ("T"     'dired-do-touch)
  ("z"     'dired-unmark)
  ("Z"     'dired-unmark-all-marks)
  ("b"     (dired-mark-extension '("elc" "go")))
  ("d"     'dired-display-file)
  ("C-d"   'dired-find-file-other-window)
  ("C-l"   'dired-omit-mode)
  ("c 0"   'al/default-directory-to-kill-ring)
  ("c RET" 'al/dired-copy-filename-as-kill)
  ("r"     'dired-do-query-replace-regexp)
  ("<ctrl-m> p" 'emms-play-dired)
  ("<ctrl-m> a" 'emms-add-dired))

(al/bind-keys
  :map dired-mode-map
  :prefix-map al/dired-isearch-map
  :prefix-doc "Map for isearch in dired."
  :prefix-key "M-s"
  ("s" 'dired-do-isearch)
  ("r" 'dired-do-isearch-regexp)
  ("f" 'dired-isearch-filenames-regexp)
  ("F" 'dired-isearch-filenames))

(al/bind-keys
  :map dired-mode-map
  :prefix-map al/dired-open-file-map
  :prefix-doc "Map for opening files in external programs in dired."
  :prefix-key "C-j"
  ("M-j" (al/dired-start-process "xdg-open"))
  ("C-j" 'al/dired-open-file)
  ("v d" (al/dired-start-process "baobab"))
  ("v f" (al/dired-start-process "gdmap" "-f"))
  ("m"   (al/dired-start-process "mupdf"))
  ("z"   (al/dired-start-process "zathura"))
  ("s"   (al/dired-start-process-on-marked-files "sxiv"))
  ("b"   'al/dired-browse-url))

(setq
 dired-auto-revert-buffer 'dired-directory-changed-p
 dired-dwim-target t
 dired-listing-switches  "-alvDh --group-directories-first"
 ;; Do not ask about copying/deleting directories.
 dired-recursive-copies  'always
 dired-recursive-deletes 'always)

(setq
 ;; Do not show "hidden" files only.
 dired-omit-files "^\\..*"
 dired-omit-extensions nil)

(setq
 dired-guess-shell-alist-user
 `((,(al/file-regexp "jpg" "png" "gif") "sxiv" "eog")
   (,(al/file-regexp "tif" "tiff") "sxiv" "evince" "eog")
   (,(al/file-regexp "pdf") "zathura" "mupdf")
   (,(al/file-regexp "djvu" "djv") "zathura")
   (,(al/file-regexp "wav" "oga" "ogg")
    "play -q" "aplay" "mpv --really-quiet")
   (,(al/file-regexp "odt" "doc") "lowriter")))

(setq al/dired-ignored-extensions
      (cons ".go" al/completion-ignored-extensions))

(al/call-at-hook dired-mode-hook
  hl-line-mode
  al/dired-set-completion-ignored-extensions)

(advice-add 'dired-sort-set-mode-line
  :override 'al/dired-sort-set-mode-line)

(al/mode-line-default-buffer-identification 'dired-mode)

(al/eval-after-load wdired
  (when (al/require dim)
    ;; "Dired" `mode-name' is hard-coded in
    ;; `wdired-change-to-dired-mode'.
    (advice-add 'wdired-change-to-dired-mode
      :after 'dim-set-major-name)))

(al/eval-after-load image-dired
  (al/bind-keys
    :map image-dired-thumbnail-mode-map
    ("↑"     'image-dired-backward-image)
    ("↓"     'image-dired-forward-image)
    ("C-↑"   'image-dired-previous-line)
    ("C-↓"   'image-dired-next-line)
    ("←"     'image-dired-display-previous-thumbnail-original)
    ("→"     'image-dired-display-next-thumbnail-original)
    ("C-M-m" 'image-dired-unmark-thumb-original-file)
    ("DEL"   'al/image-dired-unmark-thumb-original-file-backward)))

;;; dired.el ends here
