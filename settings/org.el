;;; org.el --- Settings for `org' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'org)
;; "org-compat.el" adds a hook to set `imenu-create-index-function' to
;; `org-imenu-get-tree', but it does this only after `imenu' is loaded.
;; This raises the following problem: if an org file is loaded and imenu
;; is not loaded yet, then `imenu-create-index-function' is still set to
;; `imenu-default-create-index-function', so after running `imenu', we
;; have a general index made by `imenu-default-create-index-function'
;; instead of a specialized index made by `org-imenu-get-tree'.  So
;; imenu is required here to be sure it is loaded before enabling
;; `org-mode'.
(require 'imenu)
(require 'al-org)
(require 'al-places)
(require 'al-general)
(require 'al-key)
(require 'al-list)

(defconst al/org-keys
  '([remap delete-char]
    [remap delete-backward-char]
    [remap forward-paragraph]
    [remap backward-paragraph]
    ("RET" . al/org-return)
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
    ("C-c C-y" . org-paste-special)))

(al/bind-keys-from-vars 'org-mode-map 'al/org-keys)

;; "/" and "_" are common for file names, so don't fontify them:
(setq org-emphasis-alist
      (al/assoc-delete-all '("/" "_") org-emphasis-alist))

(setq
 org-file-apps
 `(("\\.mm\\'" . default)
   ("\\.x?html?\\'" . tui/choose-browser)
   (,(al/file-regexp "jpg" "png" "gif") . "sxiv %s")
   (,(al/file-regexp "pdf") . "zathura %s")
   (,(al/file-regexp "djvu") . "zathura %s")
   ("\\.pdf::\\([0-9]+\\)\\'" . "zathura --page %1 %s")
   ("\\.djvu::\\([0-9]+\\)\\'" . "zathura --page %1 %s")
   (auto-mode . emacs)))

(setq
 org-modules '(ol-info)
 org-export-backends '(ascii html icalendar latex odt texinfo man)
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

;; A hack to fontify keys in "*Org Select*" buffer.  Ideally,
;; `org-mks' should be improved to prettify output in "*Org Select*"
;; buffer.
(dolist (assoc org-structure-template-alist)
  (setcar assoc (propertize (car assoc) 'face 'alect-key)))

(al/modify-syntax org-mode-syntax-table
  (?\" "\"\"")
  (?\' "\"'"))

(add-hook 'org-mode-hook #'al/set-default-paragraph)

(advice-add 'org-link-make-string
  :around #'al/org-link-set-description)

;; Do not require `al-org-emms' to avoid loading EMMS at org start.
(org-link-set-parameters
 "emms"
 :follow 'al/org-emms-play)
(org-link-set-parameters
 "emms-pl"
 :follow 'al/org-emms-playlist-play)

(al/eval-after-load al-org-emms
  (add-hook 'emms-mpv-file-loaded-hook #'al/org-emms-seek))

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

;;; org.el ends here
