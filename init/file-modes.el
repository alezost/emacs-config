;;; file-modes.el --- Modes for various file types  -*- lexical-binding: t -*-

;; Copyright © 2014–2018 Alex Kost

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

(require 'al-key)


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

(al/autoload "org"
  org-read-date
  org-open-file)

(setq org-export-backends
      '(ascii html icalendar latex odt texinfo man))
(with-eval-after-load 'org
  (when (require 'al-org nil t)
    (advice-add 'org-make-link-string
      :around #'al/org-set-link-description))
  (setq
   org-imenu-depth 6
   org-completion-use-ido t
   org-confirm-elisp-link-function nil
   org-src-fontify-natively t
   org-return-follows-link t
   org-startup-folded "showall"
   org-tags-column -54
   org-directory al/notes-dir
   org-default-notes-file (al/notes-dir-file "notes.org")
   org-url-hexify-p nil
   org-link-escape-chars '(?\[ ?\] ?\; ?\= ?\+)
   org-ellipsis " […]"
   org-file-apps
   `(("\\.mm\\'" . default)
     ("\\.x?html?\\'" al/choose-browser file)
     (,(al/file-regexp "jpg" "png" "gif") . "sxiv %s")
     ("\\.pdf\\'" . "zathura %s")
     ("\\.djvu\\'" . "zathura %s")
     ("\\.pdf::\\([0-9]+\\)\\'" . "zathura --page %1 %s")
     ("\\.djvu::\\([0-9]+\\)\\'" . "zathura --page %1 %s")
     (auto-mode . emacs)))

  (setq
   org-use-speed-commands t
   org-speed-commands-user
   '(("." . (org-speed-move-safe 'outline-previous-visible-heading))
     ("e" . (org-speed-move-safe 'outline-next-visible-heading))
     (">" . (org-speed-move-safe 'org-backward-heading-same-level))
     ("E" . (org-speed-move-safe 'org-forward-heading-same-level))
     ("n" . org-narrow-to-subtree)))

  (defconst al/org-keys
    '("<C-tab>"
      ("<M-tab>" . pcomplete)
      ("<M-return>" . org-meta-return)
      ("M->" . outline-previous-visible-heading)
      ("M-E" . outline-next-visible-heading)
      ("M-O" . org-backward-sentence)
      ("M-U" . org-forward-sentence)
      ("<C-kp-enter>" . al/org-table-next-column)
      ("<M-kp-enter>" . al/org-table-kill-rows-recalculate)
      ("<C-M-kp-enter>" . al/org-table-next-table)
      ("C-c e" . org-export-dispatch)
      ("C-c M-x" . org-copy-special)
      ("C-c C-t" . org-cut-special)
      ("C-c C-y" . org-paste-special))
    "Alist of auxiliary keys for `org-mode-map'.")
  (al/bind-keys-from-vars 'org-mode-map 'al/org-keys)

  (when (require 'al-minibuffer nil t)
    (advice-add 'org-set-tags :around #'al/complete-default))

  (org-add-link-type "pdfview" 'org-pdfview-open 'org-pdfview-export))

(defun al/autoload-org-protocol (fun files &rest args)
  "Load `org-protocol' if needed.
`org' is huge and loading it during emacs start is wasteful, but
it is needed to use `org-protocol', isn't it?  Not necessarily:
this function makes it possible to avoid requiring `org-protocol'
\(thus the whole `org') in the emacs config file.

Making this function an 'after' advice for `server-visit-files',
will do the right thing."
  (if (and (null (featurep 'org-protocol))
           (cl-find-if (lambda (spec)
                         ;; SPEC is (FILENAME . FILEPOS).
                         (string-match "org-protocol:/" (car spec)))
                       files))
      (if (require 'org-protocol nil t)
          ;; `server-visit-files' can't be called as is here, because
          ;; `org-protocol' has just been loaded and the protocol advice
          ;; is not active yet, so call `server-visit-files' outside
          ;; this body.
          (apply #'run-with-idle-timer .1 nil
                 #'server-visit-files files args)
        (message "`org-protocol' has not been loaded!"))
    (apply fun files args)))
(advice-add 'server-visit-files :around #'al/autoload-org-protocol)

(with-eval-after-load 'org-src
  (al/bind-keys
   :map org-src-mode-map
   ("C-c C-c" . org-edit-src-exit))
  (push '("shell" . shell-script) org-src-lang-modes))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        '(("n" "notes" entry (file org-default-notes-file)
           "* %T\n   %?\n"))))

(with-eval-after-load 'org-agenda
  (al/bind-keys
   :map org-agenda-mode-map
   ("." . org-agenda-previous-line)
   ("e" . org-agenda-next-line)))

(with-eval-after-load 'al-org
  (org-add-link-type "emms" 'al/org-emms-open)
  (al/add-hook-maybe 'org-store-link-functions
    'al/org-emms-store-link))

(al/autoload "org-pdfview"
  org-pdfview-open
  org-pdfview-export)


;;; Pdf tools

(setq pdf-tools-handle-upgrades nil)

(al/autoload "pdf-view" pdf-view-mode)
(with-eval-after-load 'pdf-view
  (al/bind-keys
   :map pdf-view-mode-map
   ("h" . pdf-view-previous-page-command))
  (add-hook 'pdf-view-mode-hook 'pdf-tools-enable-minor-modes)
  (require 'org-pdfview nil t))

(with-eval-after-load 'pdf-outline
  (al/clean-map 'pdf-outline-minor-mode-map)
  (al/bind-keys
   :map pdf-outline-minor-mode-map
   ("c" . pdf-outline))

  (defconst al/pdf-outline-buffer-keys
    '(("TAB" . outline-cycle)
      ("c" . pdf-outline-select-pdf-window)
      ("u" . pdf-outline-follow-link)
      ("d" . pdf-outline-display-link)
      ("q" . quit-window))
    "Alist of auxiliary keys for `pdf-outline-buffer-mode-map'.")
  (al/bind-keys-from-vars 'pdf-outline-buffer-mode-map
    '(al/lazy-moving-keys
      al/lazy-scrolling-keys
      al/pdf-outline-buffer-keys))

  (add-hook 'pdf-outline-buffer-mode-hook 'hl-line-mode))

(with-eval-after-load 'pdf-links
  (setq pdf-links-convert-pointsize-scale 0.02)

  (al/clean-map 'pdf-links-minor-mode-map)
  (al/bind-keys
   :map pdf-links-minor-mode-map
   ("u" . pdf-links-action-perform)
   ("U" . pdf-links-isearch-link)))

(with-eval-after-load 'pdf-history
  (al/clean-map 'pdf-history-minor-mode-map)
  (al/bind-keys
   :map pdf-history-minor-mode-map
   ("," . pdf-history-backward)
   ("p" . pdf-history-forward)))

(with-eval-after-load 'pdf-misc
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

(when (require 'al-file nil t)
  (al/add-to-auto-mode-alist
   '((sh-mode "/etc/profile\\'"
              "bashrc\\'")
     (conf-xdefaults-mode "\\.Xmodmap\\'")
     (conf-space-mode "\\.mailmap\\'")
     (conf-unix-mode "\\.rules\\'"
                     "\\.hwdb\\'"
                     "\\.cnf\\'"
                     "\\.map\\'"
                     "\\.inc\\'"
                     "\\.service\\'"
                     "\\.target\\'"
                     "\\.socket\\'"
                     "\\.timer\\'"
                     "\\.mount\\'")
     (conf-unix-mode (".*rc\\'"
                      "/etc/.*\\'")
                     t)
     (syslog-mode "/var/log.*\\'" t)
     (pdf-view-mode "\\.[pP][dD][fF]\\'")
     (markdown-mode "\\.mdown\\'")
     (pkgbuild-mode "PKGBUILD\\'")
     (java-mode ".*tmwa-server-data/world/map/npc/.*txt\\'")
     (gtypist-mode "\\.typ\\'")
     (gnuplot-mode "\\.plot\\'")
     (maxima-mode "\\.max\\'"))))

(with-eval-after-load 'conf-mode
  (al/add-hook-maybe 'conf-mode-hook 'hl-line-mode))

(with-eval-after-load 'image-mode
  (defconst al/image-keys
    '(("C-a" . image-bol)
      ("<ctrl-i>" . image-eol)
      ("h"   . image-previous-file)
      ("H"   . image-previous-frame)
      ("N"   . image-next-frame))
    "Alist of auxiliary keys for `image-mode-map'.")
  (al/bind-keys-from-vars 'image-mode-map 'al/image-keys))

(with-eval-after-load 'doc-view
  (setq doc-view-cache-directory "~/.cache/docview")
  (push "-r200" doc-view-ghostscript-options) ; picture resolution
  )

(with-eval-after-load 'markdown-mode
  (defconst al/markdown-keys
    '(("M->" . markdown-previous-link)
      ("M-E" . markdown-next-link))
    "Alist of auxiliary keys for `markdown-mode-map'.")
  (al/bind-keys-from-vars 'markdown-mode-map 'al/markdown-keys))

(with-eval-after-load 'tar-mode
  (setq tar-mode-show-date t)
  (defun al/tar-time-string (time)
    (format-time-string "  %d-%b-%Y" time))
  (advice-add 'tar-clip-time-string :override 'al/tar-time-string)

  (al/bind-keys
   :map tar-mode-map
   ("." . tar-previous-line)
   ("e" . tar-next-line)
   ("u" . tar-extract))

  (add-hook 'tar-mode-hook 'hl-line-mode))

(with-eval-after-load 'nxml-mode
  (defconst al/nxml-keys
    '(("C-M-." . nxml-backward-up-element)
      ("C-M-e" . nxml-down-element)
      ("C-M-o" . nxml-backward-element)
      ("C-M-u" . nxml-forward-element))
    "Alist of auxiliary keys for `nxml-mode-map'.")
  (al/bind-keys-from-vars 'nxml-mode-map 'al/nxml-keys)
  (add-hook 'nxml-mode-hook
            (lambda () (rng-validate-mode 0))))

;;; file-modes.el ends here
