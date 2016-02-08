;;; file-modes.el --- Modes for various file types

;; Copyright © 2014-2015 Alex Kost

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
  (require 'al-org nil t)
  (setq
   org-imenu-depth 6
   org-completion-use-ido t
   org-confirm-elisp-link-function nil
   ;; org-src-fontify-natively t
   org-return-follows-link t
   org-startup-folded "showall"
   org-tags-column -54
   org-directory al/notes-dir
   org-default-notes-file (al/notes-dir-file "notes.org")
   org-url-hexify-p nil
   org-link-escape-chars '(?\[ ?\] ?\; ?\= ?\+)
   org-ellipsis " […]"
   org-file-apps
   '(("\\.mm\\'" . default)
     ("\\.x?html?\\'" utl-choose-browser file)
     ("\\.png\\'" . "sxiv %s")
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

  ;; XXX Something in org fontifying is broken in emacs > 25.0
  (when (version< emacs-version "25")
    (setq org-src-fontify-natively t))

  (defconst al/org-keys
    '("<C-tab>"
      ("<M-tab>" . pcomplete)
      ("M->" . org-previous-link)
      ("M-E" . org-next-link)
      ("M-O" . org-backward-sentence)
      ("M-U" . org-forward-sentence)
      ("C-M-H-o" . org-backward-element)
      ("C-M-H-u" . org-forward-element)
      ("C-M-H-." . org-up-element)
      ("C-M-H-e" . org-down-element)
      ("<C-kp-enter>" . utl-org-table-next-column)
      ("<M-kp-enter>" . utl-org-table-kill-rows-recalculate)
      ("<C-M-kp-enter>" . utl-org-table-next-table)
      ("C-c e" . org-export-dispatch)
      ("C-c M-x" . org-copy-special)
      ("C-c C-t" . org-cut-special)
      ("C-c C-y" . org-paste-special))
    "Alist of auxiliary keys for `org-mode-map'.")
  (al/bind-keys-from-vars 'org-mode-map
    '(al/org-keys al/text-editing-keys))

  (when (require 'al-ido nil t)
    (advice-add 'org-set-tags :around #'utl-ido-disable))

  (org-add-link-type "pdfview" 'org-pdfview-open 'org-pdfview-export))

(al/eval-after-init
  (when (string= server-name "server")
    (require 'org-protocol nil t)))

(with-eval-after-load 'org-src
  (push '("shell" . shell-script) org-src-lang-modes))

(with-eval-after-load 'org-capture
  (setq org-capture-templates
        '(("n" "notes" entry (file org-default-notes-file)
           "* %T\n  %?\n"))))

(with-eval-after-load 'org-agenda
  (al/bind-keys
   :map org-agenda-mode-map
   ("." . org-agenda-previous-line)
   ("e" . org-agenda-next-line)))

(with-eval-after-load 'al-org
  (org-add-link-type "emms" 'utl-org-emms-open)
  (al/add-hook-maybe 'org-store-link-functions
    'utl-org-emms-store-link))

(al/autoload "org-pdfview"
  org-pdfview-open
  org-pdfview-export)


;;; Pdf tools

(setq pdf-tools-handle-upgrades nil)

(with-eval-after-load 'pdf-view
  (push '("\\.[pP][dD][fF]\\'" . pdf-view-mode)
        auto-mode-alist)
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

(setq auto-mode-alist
      (append
       '(("/etc/profile\\'" . sh-mode)
         ("bashrc\\'"     . sh-mode)
         ("\\.Xmodmap\\'" . conf-xdefaults-mode)
         ("\\.rules\\'"   . conf-unix-mode)
         ("\\.hwdb\\'"    . conf-unix-mode)
         ("\\.cnf\\'"     . conf-unix-mode)
         ("\\.map\\'"     . conf-unix-mode)
         ("\\.inc\\'"     . conf-unix-mode)
         ("\\.service\\'" . conf-unix-mode)
         ("\\.target\\'"  . conf-unix-mode)
         ("\\.socket\\'"  . conf-unix-mode)
         ("\\.timer\\'"   . conf-unix-mode)
         ("\\.mount\\'"   . conf-unix-mode)
         ("\\.mailmap\\'" . conf-space-mode)
         ("PKGBUILD\\'"   . pkgbuild-mode)
         (".*tmwa-server-data/world/map/npc/.*txt\\'" . java-mode)
         ("\\.typ\\'"     . gtypist-mode)
         ("\\.plot\\'"    . gnuplot-mode)
         ("\\.max\\'"     . maxima-mode))
       auto-mode-alist))
(add-to-list 'auto-mode-alist '("/var/log.*\\'" . syslog-mode) t)
(add-to-list 'auto-mode-alist '(".*rc\\'" . conf-unix-mode) t)
(add-to-list 'auto-mode-alist '("/etc/.*\\'" . conf-unix-mode) t)

(with-eval-after-load 'image-mode
  (defconst al/image-keys
    '(("C-a" . image-bol)
      ("C-п" . image-eol)
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
  (al/bind-keys-from-vars 'markdown-mode-map 'al/markdown-keys)
  (push '("\\.mdown\\'" . markdown-mode)
        auto-mode-alist))

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
