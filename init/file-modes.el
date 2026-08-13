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

(eval-when-compile
  (require 'al-aux-macros))
(require 'al-places)
(require 'al-general)
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
 ("b"   . org-switchb)
 ("i"   . org-toggle-inline-images)
 ("e"   . org-export)
 ("TAB" . org-indent-mode))

(al/eval-after-load org
  (al/load-settings "org"))


;;; Pdf tools

(al/autoload "pdf-view" pdf-view-mode)

(al/eval-after-load pdf-view
  (al/load-settings "pdf-tools"))


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
  (add-hook 'conf-mode-hook #'hl-line-mode))

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

  (add-hook 'tar-mode-hook #'hl-line-mode))

(al/eval-after-load nxml-mode
  (defconst al/nxml-keys
    '(("C-M-." . nxml-backward-up-element)
      ("C-M-e" . nxml-down-element)
      ("C-M-o" . nxml-backward-element)
      ("C-M-u" . nxml-forward-element))
    "Alist of auxiliary keys for `nxml-mode-map'.")
  (al/bind-keys-from-vars 'nxml-mode-map 'al/nxml-keys)

  (al/eval-at-hook nxml-mode-hook
    (rng-validate-mode 0)))

(al/eval-after-load sgml-mode
  ;; Bind default keys to get rid of "M-o" key binding there.
  (al/bind-keys-from-vars 'html-mode-map))

;;; file-modes.el ends here
