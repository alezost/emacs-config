;;; files.el --- Working with files, dired, etc.  -*- lexical-binding: t -*-

;; Copyright © 2014–2026 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Code:

(eval-when-compile
  (require 'al-aux-macros))
(require 'al-places)
(require 'al-general)
(require 'al-key)

(declare-function al/find-file "al-file-cmd")


;;; Global keys

(al/bind-key* "M-C-f" find-file-at-point)

(al/bind-keys*
  :prefix-map al/find-file-map
  :prefix-doc "Map for finding files."
  :prefix-key "C-f"
  ("C-f"   . find-file)
  ("p"     . al/find-file-in-path)
  ("S"     . al/sudo-find-file)
  ("h"     . al/ssh-find-file)
  ("z"     . al/router-get-log)
  ("u"     . al/browse-url)
  ("y"     . al/browse-youtube-video)
  ("l"     . find-library)
  ("r"     . recentf-open)
  ("e"       (al/find-file al/emacs-dir))
  ("C-c"     (al/find-file al/emacs-init-dir))
  ("C-s"     (al/find-file (al/emacs-init-dir-file "settings.el")))
  ("k"       (al/find-file (al/emacs-init-dir-file "keys.el")))
  ("i"       (al/find-file (al/emacs-init-dir-file "init.el")))
  ("t"       (al/find-file (al/emacs-init-dir-file "text.el")))
  ("v"       (al/find-file (al/emacs-init-dir-file "visual.el")))
  ("c"       (al/find-file (al/emacs-my-packages-dir-file "alect-themes")))
  ("C-M-c"   (al/find-file (al/emacs-my-packages-dir-file
                            "alect-themes/alect-themes.el"))))

(al/bind-keys
 :prefix-map al/bookmark-map
 :prefix-doc "Map for bookmarks and finding files."
 :prefix-key "M-f"
 ("M-f"   . bookmark-jump)
 ("n"     . bookmark-set)
 ("k"     . bookmark-delete)
 ("l"     . bookmark-bmenu-list)
 ("S"     . al/sr-toggle)
 ("q"       (al/find-file
             (al/src-dir-file "emacs/melpa/recipes")))
 ("h"       (al/find-file "~"))
 ("d"       (al/find-file al/journal-dir))
 ("w"       (al/find-file al/download-dir))
 ("M-d"     (find-file al/download-dir))
 ("M-n"     (al/find-file al/notes-dir))
 ("t"       (al/find-file al/tmp-dir))
 ("m"       (al/find-file al/music-dir))
 ("p"       (al/find-file al/progs-dir))
 ("b"       (al/find-file (al/config-dir-file "shell")))
 ("g"       (al/find-file (al/config-dir-file "guile")))
 ("M-c"     (al/find-file al/config-dir))
 ("C-M-c"   (find-file (al/config-dir-file "config.scm")))
 ("M-g"     (al/find-file al/guix-profile-dir))
 ("s"       (al/find-file (al/config-dir-file "stumpwm")))
 ("v"       (al/find-file "/var/log")))

(al/bind-keys
 :prefix-map al/grep-find-map
 :prefix-doc "Map for find/grep commands."
 :prefix-key "M-F"
 ("g" . grep)
 ("n" . find-name-dired)
 ("a" . find-dired)
 ("f" . grep-find))


;;; Backup and autosave

(let ((dir (al/emacs-data-dir-file "auto-save")))
  ;; Emacs does not create a directory of an autosave file and just
  ;; complains when it doesn't exist.
  (unless (file-exists-p dir)
    (al/with-demoted-errors "Making auto-save directory failed: %S"
      (mkdir dir t)))
  (setq auto-save-file-name-transforms
        `((".*" ,(file-name-as-directory dir) t))))

(setq
 auto-save-list-file-prefix
 (al/emacs-data-dir-file "auto-save-list/.saves-")
 backup-directory-alist
 `( ;;(,tramp-file-name-regexp . nil)
   (".*" . ,(al/emacs-data-dir-file "backup")))
 backup-by-copying t        ; overwrite backups, not original files
 version-control t
 kept-old-versions 2
 kept-new-versions 4
 delete-old-versions t)

(al/eval-after-load al-backup
  :load after-init
  (setq
   al/backup-ignored-regexps
   '("gnus/mail/archive/sent"
     "COMMIT_EDITMSG")
   backup-enable-predicate #'al/backup-enable-predicate)
  (advice-add 'make-backup-file-name-1
    :override #'al/make-backup-file-name-1))


;;; Dired

;; Set it before loading `dired' to avoid extra process call (executed
;; by `dired-guess-shell-gnutar' defcustom).
(al/setq-no-warnings dired-guess-shell-gnutar "tar")

(al/bind-key "H-j" dired-jump)

(al/eval-after-load dired
  (al/load-settings "dired"))


;;; Misc settings and packages

(setq
 grep-command "grep -nHi -e "
 enable-local-variables :safe
 enable-dir-local-variables nil
 ;; safe-local-variable-values '((lexical-binding . t))
 ;; enable-local-eval nil
 )

(al/eval-after-load mailcap
  ;; Use "sxiv" instead of "display" to open image files.  Actually,
  ;; (mailcap-add "image/.*" "sxiv %s") can be used, but it adds the
  ;; entry to the beginning of "image" alist, while I want to fallback
  ;; to "sxiv" as it is done with "display".
  (let* ((image-alist   (cdr (assoc "image" mailcap-mime-data)))
         (display-alist (cdr (assoc ".*" image-alist))))
    (setcdr (assq 'viewer display-alist) "sxiv %s")))

(with-eval-after-load "mule-cmds" ; there is no `mule-cmds' feature
  (defconst al/mule-keys
    '(("d" (revert-buffer-with-coding-system 'cp855))
      ("w" (revert-buffer-with-coding-system 'cp1251)))
    "Alist of auxiliary keys for `mule-keymap'.")
  (al/bind-keys-from-vars 'mule-keymap 'al/mule-keys))

(al/eval-after-load bookmark
  (al/load-settings "bookmark"))

(al/bind-keys
 :prefix-map al/recentf-map
 :prefix-doc "Map for recent files."
 :prefix-key "C-x r"
 ("m" . recentf-mode)
 ("f" . recentf-open)
 ("l" . recentf-edit-list)
 ("c" . recentf-cleanup))

(al/eval-after-load recentf
  (setq
   recentf-exclude (list (al/file-regexp "el" "gz")
                         #'file-remote-p)
   recentf-keep (list #'file-exists-p)
   recentf-used-hooks nil
   recentf-auto-cleanup 'never
   recentf-max-saved-items 300
   recentf-save-file (al/emacs-data-dir-file "recentf")))

(al/eval-after-load ffap
  (al/require al-ffap))

(al/eval-after-load al-ffap
  (advice-add 'ffap-read-file-or-url
    :override #'al/ffap-read-file-or-url))

(al/eval-after-load saveplace
  (setq
   ;; For some reason, `save-place-loaded' is t after `saveplace' load.
   ;; This bug(?) appeared somewhere between Emacs 29.4 and Emacs 30.1.
   ;; Set this variable back to nil.  Otherwise, `save-place-alist' is
   ;; empty because `save-place-file' is never loaded.
   save-place-loaded nil
   save-place-ignore-files-regexp
   (rx-to-string `(or (and string-start "/gnu")
                      (regexp ,save-place-ignore-files-regexp))
                 'no-group)
   save-place-forget-unreadable-files nil
   save-place-file (al/emacs-data-dir-file "save-places")
   save-place-limit 999)

  (al/require al-saveplace))

(al/eval-after-load al-saveplace
  (advice-add 'save-places-to-alist
    :override #'al/save-places-to-alist))

(al/eval-after-load al-file-cmd
  (setq
   al/ssh-default-user (list user-login-name "root" "lena")
   al/ssh-default-host "hyperion"))

;;; files.el ends here
