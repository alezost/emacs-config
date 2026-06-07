;;; tui-web-search.el --- Transient interface for web search  -*- lexical-binding: t -*-

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

(defvar tui/web-search-text nil
  "Current text to search for.")

(defun tui/web-search-text ()
  (al/with-face 'font-lock-string-face
    (or tui/web-search-text "")))

(defun tui/web-search-set-text (&optional text)
  (setq tui/web-search-text
        (or text
            (when (region-active-p)
              (buffer-substring-no-properties
               (region-beginning)
               (region-end))))))

(transient-define-suffix tui/web-search:text ()
  (interactive)
  (tui/web-search (read-string "Text: " tui/web-search-text)))

(defmacro tui/web-search-define-suffix (&rest names)
  "Generate `tui/web-search-NAME' transient suffixes for all engine NAMES."
  (declare (indent 0) (debug t))
  `(progn
     ,@(mapcar
        (lambda (name)
          (let* ((name-str     (symbol-name name))
                 (suf-name-str (concat "tui/web-search:" name-str))
                 (ws-name-str  (concat "web-search-" name-str))
                 (suf-name     (intern suf-name-str))
                 (ws-name      (intern ws-name-str)))
            `(transient-define-suffix ,suf-name ()
               ,(concat "Search for `tui/web-search-text' using `"
                        ws-name-str "'.")
               (interactive)
               (,ws-name (or tui/web-search-text
                             (web-search-prompt-for-string))))))
        names)))

;; Suppress byte-compile warnings as some of the following web-search
;; functions are defined in my config.
(with-no-warnings
  (tui/web-search-define-suffix
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

(transient-define-suffix tui/web-search:any ()
  "Search for some text using some web search engine."
  (interactive)
  (web-search (or tui/web-search-text
                  (web-search-prompt-for-string))
              (web-search-prompt-for-engine)))

;;;###autoload (autoload 'tui/web-search "tui-web-search" nil t)
(transient-define-prefix tui/web-search (&optional text)
  "Interface for web search."
  ["Text"
   (:info #'tui/web-search-text :format "%d")
   ("T" "set text" tui/web-search:text)]
  ["Web search"
   [("d" "DuckDuckGo" tui/web-search:duckduckgo)
    ("g" "Google" tui/web-search:google)
    ("Y" "Yandex" tui/web-search:yandex)]
   [("we" "Wikipedia (en)" tui/web-search:wikipedia-en)
    ("wr" "Wikipedia (ru)" tui/web-search:wikipedia-ru)
    ("wi" "Wiktionary (en)" tui/web-search:wiktionary-en)]
   [("e" "EmacsWiki" tui/web-search:emacswiki)
    ("a" "ArchWiki" tui/web-search:archwiki)
    ("A" "Arch Package" tui/web-search:arch-package)]
   [("y" "YouTube" tui/web-search:youtube)]
   [("i" "IPDuh" tui/web-search:ipduh)
    ("I" "IP Address" tui/web-search:ip-address)]
   [("G" "Github" tui/web-search:github)
    ("b" "Debbugs" tui/web-search:debbugs)
    ("M-S" "Other" tui/web-search:any)]]
  (interactive)
  (tui/web-search-set-text text)
  (transient-setup 'tui/web-search))

(provide 'tui-web-search)

;;; tui-web-search.el ends here
