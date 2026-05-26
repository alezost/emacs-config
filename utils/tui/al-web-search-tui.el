;;; al-web-search-tui.el --- Transient interface for web search  -*- lexical-binding: t -*-

;; Copyright © 2026 Alex Kost

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

(require 'transient)
(require 'web-search)
(require 'al-visual)

(defvar al/web-search-tui-text nil
  "Current text to search for.")

(defun al/web-search-tui-text ()
  (al/with-face 'font-lock-string-face
    (or al/web-search-tui-text "")))

(defun al/web-search-tui-set-text (&optional text)
  (setq al/web-search-tui-text
        (or text
            (when (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))))))

(transient-define-suffix al/web-search-tui:text ()
  (interactive)
  (al/web-search-tui (read-string "Text: " al/web-search-tui-text)))

(defmacro al/web-search-tui-define-suffix (&rest names)
  "Generate `al/web-search-tui-NAME' transient suffixes for all engine NAMES."
  (declare (indent 0) (debug t))
  `(progn
     ,@(mapcar
        (lambda (name)
          (let* ((name-str     (symbol-name name))
                 (suf-name-str (concat "al/web-search-tui:" name-str))
                 (ws-name-str  (concat "web-search-" name-str))
                 (suf-name     (intern suf-name-str))
                 (ws-name      (intern ws-name-str)))
            `(transient-define-suffix ,suf-name ()
               ,(concat "Search for `al/web-search-tui-text' using `"
                        ws-name-str "'.")
               (interactive)
               (,ws-name (or al/web-search-tui-text
                             (web-search-prompt-for-string))))))
        names)))

;; Suppress byte-compile warnings as some of the following web-search
;; functions are defined in my config.
(with-no-warnings
  (al/web-search-tui-define-suffix
    duckduckgo
    google
    yandex
    youtube
    github
    wikipedia-en
    wikipedia-ru
    wiktionary-en
    emacswiki
    archwiki
    arch-package
    ipduh
    ip-address
    debbugs))

(transient-define-suffix al/web-search-tui:any ()
  "Search for some text using some web search engine."
  (interactive)
  (web-search (or al/web-search-tui-text
                  (web-search-prompt-for-string))
              (web-search-prompt-for-engine)))

;;;###autoload (autoload 'al/web-search-tui "al-web-search-tui" nil t)
(transient-define-prefix al/web-search-tui (&optional text)
  "Interface for web search."
  ["Text"
   (:info #'al/web-search-tui-text :format "%d")
   ("T" "set text" al/web-search-tui:text)]
  ["Web search"
   [("d" "DuckDuckGo" al/web-search-tui:duckduckgo)
    ("g" "Google" al/web-search-tui:google)
    ("Y" "Yandex" al/web-search-tui:yandex)]
   [("we" "Wikipedia (en)" al/web-search-tui:wikipedia-en)
    ("wr" "Wikipedia (ru)" al/web-search-tui:wikipedia-ru)
    ("wi" "Wiktionary (en)" al/web-search-tui:wiktionary-en)]
   [("e" "EmacsWiki" al/web-search-tui:emacswiki)
    ("a" "ArchWiki" al/web-search-tui:archwiki)
    ("A" "Arch Package" al/web-search-tui:arch-package)]
   [("y" "YouTube" al/web-search-tui:youtube)]
   [("i" "IPDuh" al/web-search-tui:ipduh)
    ("I" "IP Address" al/web-search-tui:ip-address)]
   [("G" "Github" al/web-search-tui:github)
    ("b" "Debbugs" al/web-search-tui:debbugs)
    ("M-S" "Other" al/web-search-tui:any)]]
  (interactive)
  (al/web-search-tui-set-text text)
  (transient-setup 'al/web-search-tui))

(provide 'al-web-search-tui)

;;; al-web-search-tui.el ends here
