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

(bind-keys
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

(use-package org
  :defer t
  :idle
  (and (utl-server-running-p)
       (require 'org-protocol nil t))

  :config
  (require 'utl-org nil t)
  (setq
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
   org-file-apps
   '(("\\.mm\\'" . default)
     ("\\.x?html?\\'" utl-choose-browser file)
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
     ("E" . (org-speed-move-safe 'org-forward-heading-same-level))))

  ;; XXX Something in org fontifying is broken in emacs >24.4.50.
  (when (version< emacs-version "24.4.50")
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
      ("C-c M-x" . org-copy-special)
      ("C-c C-t" . org-cut-special)
      ("C-c C-y" . org-paste-special))
    "Alist of auxiliary keys for `org-mode-map'.")
  (al/bind-keys-from-vars 'org-mode-map
    '(al/org-keys al/text-editing-keys))

  (when (require 'utl-ido nil t)
    (advice-add 'org-set-tags :around #'utl-ido-disable)))

(use-package org-src
  :defer t
  :config
  (push '("shell" . shell-script) org-src-lang-modes))

(use-package org-capture
  :defer t
  :config
  (setq org-capture-templates
        '(("n" "notes" entry (file org-default-notes-file)
           "* %T\n  %?\n"))))

(use-package org-agenda
  :defer t
  :config
  (bind-keys
   :map org-agenda-mode-map
   ("." . org-agenda-previous-line)
   ("e" . org-agenda-next-line)))

(use-package utl-org
  :defer t
  :config
  (org-add-link-type "emms" 'utl-org-emms-open)
  (al/add-hook-maybe 'org-store-link-functions
    'utl-org-emms-store-link))


;;; Misc settings and packages

(setq auto-mode-alist
      (append
       '(("/var/log.*\\'" . syslog-mode)
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
         ("PKGBUILD\\'"   . pkgbuild-mode)
         (".*tmwa-server-data/world/map/npc/.*txt\\'" . java-mode)
         ("\\.typ\\'"     . gtypist-mode)
         ("\\.plot\\'"    . gnuplot-mode)
         ("\\.max\\'"     . maxima-mode))
       auto-mode-alist))
(add-to-list 'auto-mode-alist '(".*rc\\'" . conf-unix-mode) t)
(add-to-list 'auto-mode-alist '("/etc/.*\\'" . conf-unix-mode) t)

(use-package markdown-mode
  :defer t
  :mode "\\.mdown\\'"
  :config
  (defconst al/markdown-keys
    '(("M->" . markdown-previous-link)
      ("M-E" . markdown-next-link))
    "Alist of auxiliary keys for `markdown-mode-map'.")
  (al/bind-keys-from-vars 'markdown-mode-map 'al/markdown-keys))

(use-package tar-mode
  :defer t
  :config
  (setq tar-mode-show-date t)
  (defun al/tar-time-string (time)
    (format-time-string "  %d-%b-%Y" time))
  (defalias 'tar-clip-time-string 'al/tar-time-string)

  (bind-keys
   :map tar-mode-map
   ("." . tar-previous-line)
   ("e" . tar-next-line)
   ("u" . tar-extract)))

(use-package nxml-mode
  :defer t
  :config
  (add-hook 'nxml-mode-hook
            (lambda () (rng-validate-mode 0))))

;;; file-modes.el ends here
