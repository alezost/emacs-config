;;; file-modes.el --- Modes for various file types  -*- lexical-binding: t -*-

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

(require 'al-places)
(require 'al-general)
(require 'al-key)

(declare-function al/file-regexp "al-file")


;;; Org

(al/bind-keys
 :prefix-map al/org-map
 :prefix-docstring "Map for org mode."
 :prefix "M-r"
 ("M-r" . org-insert-link)
 ("l"   . org-store-link)
 ("M-l" . org-store-link)
 ("M-b" . org-mark-ring-goto)
 ("c"   . org-capture)
 ("a"   . org-agenda)
 ("b"   . org-iswitchb)
 ("i"   . org-toggle-inline-images)
 ("e"   . org-export)
 ("TAB" . org-indent-mode))

(al/eval-after-load al-org
  (advice-add 'org-link-make-string
    :around #'al/org-link-set-description))

(al/eval-after-load al-org-emms
  (when (al/require emms-mpv)
    (add-hook 'emms-mpv-file-loaded-hook #'al/org-emms-seek)))

(al/autoload "org"
  org-read-date
  org-open-file)

(al/setq-no-warnings
 org-export-backends
 '(ascii html icalendar latex odt texinfo man))

(al/eval-after-load org
  (when (al/require al-text)
    (al/add-hook-maybe 'org-mode-hook 'al/set-default-paragraph))

  ;; "/" and "_" are common for file names, so don't fontify them:
  (setq org-emphasis-alist
        (al/assoc-delete-all '("/" "_") org-emphasis-alist))

  (when (al/require al-file)
    (setq
     org-file-apps
     `(("\\.mm\\'" . default)
       ("\\.x?html?\\'" . al/choose-browser)
       (,(al/file-regexp "jpg" "png" "gif") . "sxiv %s")
       (,(al/file-regexp "pdf") . "zathura %s")
       (,(al/file-regexp "djvu") . "zathura %s")
       ("\\.pdf::\\([0-9]+\\)\\'" . "zathura --page %1 %s")
       ("\\.djvu::\\([0-9]+\\)\\'" . "zathura --page %1 %s")
       (auto-mode . emacs))))

  (setq
   org-modules '(ol-info)
   org-imenu-depth 6
   org-link-elisp-confirm-function nil
   org-src-fontify-natively t
   org-fontify-quote-and-verse-blocks t
   org-return-follows-link t
   org-startup-folded "showall"
   org-tags-column -54
   org-directory al/notes-dir
   org-default-notes-file (al/notes-dir-file "notes.org")
   org-ellipsis " […]"

   org-use-speed-commands t
   org-speed-commands
   (append '(("." . (org-speed-move-safe 'org-previous-visible-heading))
             ("e" . (org-speed-move-safe 'org-next-visible-heading))
             (">" . (org-speed-move-safe 'org-backward-heading-same-level))
             ("E" . (org-speed-move-safe 'org-forward-heading-same-level))
             ("n" . org-narrow-to-subtree))
           org-speed-commands))

  (al/modify-syntax org-mode-syntax-table
    (?\" "\"\"")
    (?\' "\"'"))

  ;; A hack to fontify keys in "*Org Select*" buffer.  Ideally,
  ;; `org-mks' should be improved to prettify output in "*Org Select*"
  ;; buffer.
  (dolist (assoc org-structure-template-alist)
    (setcar assoc (propertize (car assoc) 'face 'alect-key)))

  (defconst al/org-keys
    '([remap delete-char]
      [remap delete-backward-char]
      [remap forward-paragraph]
      [remap backward-paragraph]
      ("TAB" . al/org-tab)
      ("M-<return>" . org-meta-return)
      ("M->" . outline-previous-visible-heading)
      ("M-E" . outline-next-visible-heading)
      ("M-O" . org-backward-sentence)
      ("M-U" . org-forward-sentence)
      ("<C-kp-enter>" . al/org-table-next-column)
      ("<M-kp-enter>" . al/org-table-kill-rows-recalculate)
      ("<C-M-kp-enter>" . al/org-table-next-table)
      ("C-j" . al/org-return-indent)
      ("C-c e" . org-export-dispatch)
      ("C-c C-b" . org-insert-structure-template)
      ("C-c M-x" . org-copy-special)
      ("C-c C-t" . org-cut-special)
      ("C-c C-y" . org-paste-special))
    "Alist of auxiliary keys for `org-mode-map'.")
  (al/bind-keys-from-vars 'org-mode-map 'al/org-keys)

  ;; Do not require `al-org-emms' to avoid loading EMMS at org start.
  (org-link-set-parameters
   "emms"
   :follow #'al/org-emms-play)
  (org-link-set-parameters
   "emms-pl"
   :follow #'al/org-emms-playlist-play)

  (al/require
    ;; "org-compat.el" adds a hook to set `imenu-create-index-function' to
    ;; `org-imenu-get-tree', but it does this only after `imenu' is loaded.
    ;; This raises the following problem: if an org file is loaded and
    ;; imenu is not loaded yet, then `imenu-create-index-function' is still
    ;; set to `imenu-default-create-index-function', so after running
    ;; `imenu', we have a general index made by
    ;; `imenu-default-create-index-function' instead of a specialized index
    ;; made by `org-imenu-get-tree'.  So imenu is required here to be sure
    ;; it is loaded before enabling `org-mode'.
    imenu
    al-org))

(al/eval-after-load org-src
  (setq org-edit-src-content-indentation 0)
  (al/bind-keys
    :map org-src-mode-map
    ("C-c C-c" . org-edit-src-exit))
  (push '("shell" . shell-script) org-src-lang-modes))

(al/eval-after-load org-capture
  (setq org-capture-templates
        '(("n" "notes" entry (file org-default-notes-file)
           "* %T\n   %?\n"))))

(al/eval-after-load org-agenda
  (al/bind-keys
   :map org-agenda-mode-map
   ("." . org-agenda-previous-line)
   ("e" . org-agenda-next-line)))

(al/eval-after-load org-ref
  (defvar al/org-ref-cite-keys
    '("H-o" "H-u" "H-e"))
  (al/bind-keys-from-vars 'org-ref-cite-keymap 'al/org-ref-cite-keys))


;;; Pdf tools

(al/setq-no-warnings pdf-tools-handle-upgrades nil)

(al/autoload "pdf-view" pdf-view-mode)

(al/eval-after-load pdf-view
  (when (al/require al-pdf)
    (advice-add 'pdf-view-deactivate-region
      :override 'al/pdf-view-deactivate-region))

  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-view-mode-hook
        '(pdf-history-minor-mode
          pdf-isearch-minor-mode
          pdf-links-minor-mode
          pdf-misc-minor-mode
          pdf-outline-minor-mode
          pdf-misc-context-menu-minor-mode
          pdf-cache-prefetch-minor-mode
          pdf-occur-global-minor-mode))

  (al/bind-keys
    :map pdf-view-mode-map
    ("h" . al/pdf-view-previous-page)
    ("n" . al/pdf-view-next-page)
    ("c" . pdf-view-themed-minor-mode)
    ([down-mouse-1] . al/pdf-view-select-region)
    ([double-mouse-1] . al/pdf-view-select-word)))

(al/eval-after-load pdf-outline
  (al/clean-map 'pdf-outline-minor-mode-map)
  (al/bind-keys
   :map pdf-outline-minor-mode-map
   ("i" . pdf-outline))

  (defconst al/pdf-outline-buffer-keys
    '(("TAB" . outline-cycle)
      ("i" . pdf-outline-select-pdf-window)
      ("u" . pdf-outline-follow-link)
      ("d" . pdf-outline-display-link)
      ("q" . quit-window))
    "Alist of auxiliary keys for `pdf-outline-buffer-mode-map'.")
  (al/bind-keys-from-vars 'pdf-outline-buffer-mode-map
    '(al/lazy-moving-keys
      al/lazy-scrolling-keys
      al/pdf-outline-buffer-keys))

  (add-hook 'pdf-outline-buffer-mode-hook 'hl-line-mode))

(al/eval-after-load pdf-links
  (setq pdf-links-convert-pointsize-scale 0.02)

  (al/clean-map 'pdf-links-minor-mode-map)
  (al/bind-keys
   :map pdf-links-minor-mode-map
   ("u" . pdf-links-action-perform)
   ("U" . pdf-links-isearch-link)))

(al/eval-after-load pdf-history
  (al/clean-map 'pdf-history-minor-mode-map)
  (al/bind-keys
   :map pdf-history-minor-mode-map
   ("," . pdf-history-backward)
   ("p" . pdf-history-forward)))

(al/eval-after-load pdf-misc
  (al/clean-map 'pdf-misc-minor-mode-map)
  (al/bind-keys
   :map pdf-misc-minor-mode-map
   ("f" . pdf-misc-display-metadata)
   ("F" . pdf-misc-display-metadata)))


;;; Misc settings and packages

;; `normal-mode' should always be called with t argument, otherwise
;; it simply ignores the value of `enable-local-variables' and sets
;; it to t.
(defun al/fix-normal-mode (&rest _)
  (list t))
(advice-add 'normal-mode :filter-args #'al/fix-normal-mode)

(al/eval-after-load al-file
  :load t
  (al/add-to-auto-mode-alist
   `((sh-mode "/etc/profile\\'"
              "bashrc\\'")
     (conf-xdefaults-mode ,(al/file-regexp "Xmodmap"))
     (conf-space-mode ,(al/file-regexp "mailmap" "gitignore"))
     (conf-unix-mode ,(al/file-regexp
                       "rules" "hwdb" "cnf" "map" "inc" "service"
                       "target" "socket" "timer" "mount"))
     (conf-unix-mode (".*rc\\'"
                      "/etc/.*\\'")
                     t)
     (js-mode "/etc/polkit-1/rules\\.d/.+\\.rules")
     (syslog-mode ("/var/log.*\\'"
                   ;;"\\.log\\'" not this because of ~/config/emacs/data/emacs-wget.log
                   )
                  t)
     (zapret-nfqws-mode "zapret.*\\.conf\\'")
     (emacs-lisp-mode "/emms/.+\\.pl\\'") ; my playlists in `emms-directory'
     (pdf-view-mode "\\.[pP][dD][fF]\\'")
     (markdown-mode ,(al/file-regexp "mdown"))
     (pkgbuild-mode "PKGBUILD\\'")
     (java-mode ".*tmwa-server-data/world/map/npc/.*txt\\'")
     (gtypist-mode ,(al/file-regexp "typ"))
     (gnuplot-mode ,(al/file-regexp "plot"))
     (maxima-mode ,(al/file-regexp "max")))))

(al/eval-after-load conf-mode
  (al/add-hook-maybe 'conf-mode-hook 'hl-line-mode))

(al/eval-after-load image-mode
  (defconst al/image-keys
    '(("C-a" . image-bol)
      ("<ctrl-i>" . image-eol)
      ("h"   . image-previous-file)
      ("H"   . image-previous-frame)
      ("N"   . image-next-frame))
    "Alist of auxiliary keys for `image-mode-map'.")
  (al/bind-keys-from-vars 'image-mode-map 'al/image-keys))

(al/eval-after-load doc-view
  (setq doc-view-cache-directory "~/.cache/docview")
  (push "-r200" doc-view-ghostscript-options) ; picture resolution
  )

(al/eval-after-load markdown-mode
  (defconst al/markdown-keys
    '(("M->" . markdown-previous-link)
      ("M-E" . markdown-next-link))
    "Alist of auxiliary keys for `markdown-mode-map'.")
  (al/bind-keys-from-vars 'markdown-mode-map 'al/markdown-keys))

(al/eval-after-load tar-mode
  (setq tar-mode-show-date t)

  (al/bind-keys
   :map tar-mode-map
   ("." . tar-previous-line)
   ("e" . tar-next-line)
   ("u" . tar-extract))

  (add-hook 'tar-mode-hook 'hl-line-mode))

(al/eval-after-load nxml-mode
  (defconst al/nxml-keys
    '(("C-M-." . nxml-backward-up-element)
      ("C-M-e" . nxml-down-element)
      ("C-M-o" . nxml-backward-element)
      ("C-M-u" . nxml-forward-element))
    "Alist of auxiliary keys for `nxml-mode-map'.")
  (al/bind-keys-from-vars 'nxml-mode-map 'al/nxml-keys)
  (add-hook 'nxml-mode-hook
            (lambda () (rng-validate-mode 0))))

(al/eval-after-load sgml-mode
  ;; Bind default keys to get rid of "M-o" key binding there.
  (al/bind-keys-from-vars 'html-mode-map))

;;; file-modes.el ends here
