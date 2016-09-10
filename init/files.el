;;; files.el --- Working with files; dired, sunrise-commander, …

;; Copyright © 2014-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(require 'al-key)


;;; Global keys

(al/bind-key* "M-C-f" find-file-at-point)

(al/bind-keys*
 :prefix-map al/find-file-map
 :prefix-docstring "Map for finding files."
 :prefix "C-f"
 ("C-f"   . ido-find-file)
 ("S"     . al/sudo-find-file)
 ("h"     . al/ssh-find-file)
 ("z"     . al/router-get-log)
 ("u"     . browse-url-emacs)
 ("l"     . find-library)
 ("e"       (al/ido-find-file al/emacs-dir))
 ("C-c"     (al/ido-find-file al/emacs-init-dir))
 ("C-s"     (find-file (al/emacs-init-dir-file "settings.el")))
 ("k"       (find-file (al/emacs-init-dir-file "keys.el")))
 ("i"       (find-file (al/emacs-init-dir-file "init.el")))
 ("t"       (find-file (al/emacs-init-dir-file "text.el")))
 ("v"       (find-file (al/emacs-init-dir-file "visual.el")))
 ("c"       (al/ido-find-file (al/emacs-my-packages-dir-file "alect-themes")))
 ("C-M-c"   (find-file (al/emacs-my-packages-dir-file
                        "alect-themes/alect-themes.el"))))

(al/bind-keys
 :prefix-map al/bookmark-map
 :prefix-docstring "Map for bookmarks and finding files."
 :prefix "M-f"
 ("M-f"   . bookmark-jump)
 ("n"     . bookmark-set)
 ("k"     . bookmark-delete)
 ("l"     . bookmark-bmenu-list)
 ("S"     . al/sr-toggle)
 ("q"       (al/ido-find-file
             (expand-file-name "package-build/recipes/"
                               quelpa-build-dir)))
 ("h"       (al/ido-find-file "~"))
 ("d"       (al/ido-find-file al/journal-dir))
 ("w"       (al/ido-find-file al/download-dir))
 ("e"       (find-file al/echo-download-dir))
 ("M-n"     (al/ido-find-file al/notes-dir))
 ("t"       (al/ido-find-file al/tmp-dir))
 ("m"       (al/ido-find-file al/music-dir))
 ("p"       (al/ido-find-file al/progs-dir))
 ("b"       (al/ido-find-file (al/config-dir-file "shell")))
 ("g"       (al/ido-find-file (al/config-dir-file "guile")))
 ("M-c"     (al/ido-find-file al/config-dir))
 ("C-M-c"   (find-file (al/config-dir-file "config.scm")))
 ("M-g"     (al/ido-find-file al/guix-profile-dir))
 ("c"       (al/ido-find-file (al/config-dir-file "conkeror")))
 ("s"       (al/ido-find-file (al/config-dir-file "stumpwm")))
 ("v"       (al/ido-find-file "/var/log")))

(al/bind-keys
 :prefix-map al/grep-find-map
 :prefix-docstring "Map for find/grep commands."
 :prefix "M-F"
 ("g" . grep)
 ("n" . find-name-dired)
 ("a" . find-dired)
 ("f" . grep-find))


;;; Backup and autosave

(let ((dir (al/emacs-data-dir-file "auto-save")))
  ;; Emacs does not create a directory of an autosave file and just
  ;; complains when it doesn't exist.
  (unless (file-exists-p dir)
    (with-demoted-errors "ERROR during making auto-save directory: %S"
      (mkdir dir t)))
  (setq auto-save-file-name-transforms
        `((".*" ,(file-name-as-directory dir) t))))

(setq
 auto-save-list-file-prefix
 (al/emacs-data-dir-file "auto-save-list/.saves-")
 backup-directory-alist
 `( ;;(,tramp-file-name-regexp . nil)
   (".*" . ,(al/emacs-data-dir-file "backup")))
 backup-by-copying t        ; overwrite backups, not originals files
 version-control t
 kept-old-versions 2
 kept-new-versions 4
 delete-old-versions t
 vc-make-backup-files t)

(when (require 'al-backup nil t)
  (setq backup-enable-predicate 'al/backup-enable-predicate)
  (advice-add 'make-backup-file-name-1
    :override 'al/make-backup-file-name-1))


;;; Dired

(with-eval-after-load 'dired
  (setq
   dired-auto-revert-buffer 'dired-directory-changed-p
   dired-dwim-target t
   dired-listing-switches  "-alvDh --group-directories-first"
   ;; Do not ask about copying/deleting directories.
   dired-recursive-copies  'always
   dired-recursive-deletes 'always)

  (defconst al/dired-keys
    '("c"
      ("SPC"   . al/dired-get-size)
      ("N"     . dired-create-directory)
      ("M"     . al/dired-man-or-chmod)
      ("f"     . dired-show-file-type)
      ("F"     . al/dired-stat)
      ("o"     . dired-up-directory)
      ("u"     . al/dired-find-file)
      ("U"     . dired-do-find-marked-files)
      ("."     . dired-previous-line)
      ("e"     . dired-next-line)
      (">"     . dired-prev-dirline)
      ("E"     . dired-next-dirline)
      ("C-M-." . dired-prev-dirline)
      ("C-M-e" . dired-next-dirline)
      ("H-a"   . al/dired-beginning-of-buffer)
      ("H-i"   . al/dired-end-of-buffer)
      ("M-d"   . dired-toggle-read-only)
      ("p"     . pathify-dired)
      ("t"     . image-dired-display-thumbs)
      ("T"     . dired-do-touch)
      ("z"     . dired-unmark)
      ("Z"     . dired-unmark-all-marks)
      ("b"       (dired-mark-extension "elc"))
      ("d"     . dired-display-file)
      ("C-d"   . dired-find-file-other-window)
      ("C-l"   . dired-omit-mode)
      ("c 0"   . al/default-directory-to-kill-ring)
      ("c RET"   (dired-copy-filename-as-kill 0))
      ("r"     . dired-do-query-replace-regexp)
      ("C-ь p" . emms-play-dired)
      ("C-ь a" . emms-add-dired))
    "Alist of auxiliary keys for `dired-mode'.")
  (al/bind-keys-from-vars 'dired-mode-map 'al/dired-keys)

  (al/bind-keys
   :map dired-mode-map
   :prefix-map al/dired-isearch-map
   :prefix-docstring "Map for isearch in dired."
   :prefix "M-s"
   ("s" . dired-do-isearch)
   ("r" . dired-do-isearch-regexp)
   ("f" . dired-isearch-filenames-regexp)
   ("F" . dired-isearch-filenames))

  (al/bind-keys
   :map dired-mode-map
   :prefix-map al/dired-open-file-map
   :prefix-docstring "Map for opening files in external programs in dired."
   :prefix "C-j"
   ("M-j"   (al/dired-start-process "xdg-open"))
   ("C-j" . al/dired-open-file)
   ("v d"   (al/dired-start-process "baobab"))
   ("v f"   (al/dired-start-process "gdmap" "-f"))
   ("m"     (al/dired-start-process "mupdf"))
   ("z"     (al/dired-start-process "zathura"))
   ("s"     (al/dired-start-process-on-marked-files "sxiv"))
   ("c"     (al/browse-url-conkeror
             (browse-url-file-url (dired-get-filename))))
   ("w"     (w3m-browse-url
             (browse-url-file-url (dired-get-filename)))))

  (al/add-hook-maybe 'dired-mode-hook 'hl-line-mode)

  (when (require 'al-mode-line nil t)
    (al/mode-line-default-buffer-identification 'dired-mode))

  (require 'dired-x nil t)
  (when (require 'al-dired nil t)
    (advice-add 'dired-sort-set-mode-line
      :override 'al/dired-sort-set-mode-line)))

(setq
 dired-guess-shell-gnutar "tar"
 dired-bind-jump nil
 dired-bind-man nil)
(al/bind-key "H-j" dired-jump)
(al/autoload "dired-x" dired-jump)

(with-eval-after-load 'dired-x
  (setq
   ;; Do not show "hidden" files only.
   dired-omit-files "^\\..*"
   dired-omit-extensions nil)
  ;; Do not rebind my keys!!
  (al/bind-keys-from-vars 'dired-mode-map 'al/dired-keys t)
  (when (require 'al-file nil t)
    (setq dired-guess-shell-alist-user
          `((,(al/file-regexp "jpg" "png" "gif") "sxiv" "eog")
            (,(al/file-regexp "tif" "tiff") "sxiv" "evince" "eog")
            (,(al/file-regexp "pdf") "zathura" "mupdf")
            (,(al/file-regexp "djvu" "djv") "zathura")
            (,(al/file-regexp "wav" "oga" "ogg")
             "play -q" "aplay" "mplayer -really-quiet" "mpv --really-quiet")
            (,(al/file-regexp "odt" "doc") "lowriter")))))

(with-eval-after-load 'dired-aux
  (when (require 'al-dired nil t)
    (advice-add 'dired-mark-read-file-name
      :override 'al/dired-mark-read-file-name)))

(with-eval-after-load 'wdired
  (al/bind-keys-from-vars 'wdired-mode-map)
  (when (require 'dim nil t)
    ;; "Dired" `mode-name' is hard-coded in
    ;; `wdired-change-to-dired-mode'.
    (advice-add 'wdired-change-to-dired-mode
      :after #'dim-set-major-name)))

(with-eval-after-load 'image-dired
  (al/bind-keys
   :map image-dired-thumbnail-mode-map
   ("."     . image-dired-backward-image)
   ("e"     . image-dired-forward-image)
   ("C-."   . image-dired-previous-line)
   ("C-e"   . image-dired-next-line)
   ("o"     . image-dired-display-previous-thumbnail-original)
   ("u"     . image-dired-display-next-thumbnail-original)
   ("C-M-m" . image-dired-unmark-thumb-original-file)
   ("DEL"   . al/image-dired-unmark-thumb-original-file-backward)))


;;; Misc settings and packages

(setq
 directory-free-space-args "-Ph"
 grep-command "grep -nHi -e "
 enable-local-variables :safe
 ;; safe-local-variable-values '((lexical-binding . t))
 ;; enable-local-eval nil
 )

(with-eval-after-load 'bookmark
  (setq
   bookmark-save-flag 1
   bookmark-default-file (al/emacs-data-dir-file "bookmarks"))
  (defconst al/bookmark-keys
    '(("u"   . bookmark-bmenu-relocate)
      ("d"   . bookmark-bmenu-other-window)
      ("C-d" . bookmark-bmenu-switch-other-window)
      ("R"   . bookmark-bmenu-rename)
      ("z"   . bookmark-bmenu-unmark)
      ("D"   . bookmark-bmenu-delete)
      ("M-d" . bookmark-bmenu-edit-annotation))
    "Alist of auxiliary keys for `bookmark-bmenu-mode'.")
  (al/bind-keys-from-vars 'bookmark-bmenu-mode-map
    '(al/lazy-moving-keys al/bookmark-keys)))

(with-eval-after-load 'recentf
  (setq
   recentf-keep nil
   recentf-auto-cleanup 'never
   recentf-max-saved-items 99
   recentf-save-file (al/emacs-data-dir-file "recentf")))

(with-eval-after-load 'saveplace
  (setq-default save-place t)
  (setq
   save-place-file (al/emacs-data-dir-file "save-places")
   save-place-limit 999))
(al/eval-after-init (require 'saveplace nil t))

(with-eval-after-load 'al-file-cmd
  (setq
   al/ssh-default-user (list user-login-name "root" "lena")
   al/ssh-default-host "hyperion"))

(with-eval-after-load 'sunrise-commander
  (setq
   sr-listing-switches "-alh --group-directories-first --no-group"
   sr-show-hidden-files nil
   sr-confirm-kill-viewer nil
   sr-modeline-use-utf8-marks t)
  ;; Do not block windows resizing with `sr-lock-window'.
  (remove-hook 'window-size-change-functions 'sr-lock-window)

  (defconst al/sr-keys
    '(("i"   . sr-show-files-info)
      ("o"   . sr-dired-prev-subdir)
      ("u"   . sr-advertised-find-file)
      ("M-u" . sr-advertised-find-file-other)
      (","   . sr-history-prev)
      ("p"   . sr-history-next)
      ("y"   . sr-synchronize-panes)
      ("H-a" . sr-beginning-of-buffer)
      ("H-i" . sr-end-of-buffer)
      ("V"     (sr-quick-view t)))
    "Alist of auxiliary keys for `sr-mode-map'.")
  (al/bind-keys-from-vars 'sr-mode-map 'al/sr-keys))

;;; files.el ends here
