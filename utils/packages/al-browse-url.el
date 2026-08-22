;;; al-browse-url.el --- Additional functionality for `browse-url' package  -*- lexical-binding: t -*-

;; Copyright © 2013–2026 Alex Kost

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

(require 'seq)
(require 'browse-url)
(require 'al-file)


;;; Additional browsers

(defvar al/firefox-profile-regexp
  "\\`[0-9a-z]\\{8\\}\\.\\([a-z]+\\)\\'"
  "Regexp matching Firefox profile directory name.")

(al/defun-lazy al/firefox-profiles
  "Return list of all firefox profiles."
  (seq-keep (lambda (name)
              (and (string-match al/firefox-profile-regexp name)
                   (match-string 1 name)))
            (al/subdirs "~/.mozilla/firefox" 'base)))

(defcustom al/browse-url-program "browser"
  "Shell command name for the default browser."
  :type 'string
  :group 'browse-url)

(defcustom al/browse-url-arguments nil
  "A list of strings to pass to the default browser as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

(defun al/browse-url-default (url &rest args)
  "Ask the default browser to load URL."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((url (browse-url-encode-url url))
        (process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "browser " url) nil
	   al/browse-url-program
	   (append al/browse-url-arguments
                   args
                   (list url)))))

(defun al/browse-url-firefox (url &rest args)
  "Ask Firefox browser to load URL."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((url (browse-url-encode-url url))
        (process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "firefox " url) nil
	   "firefox"
	   (append args (list url)))))

(defun al/browse-url-tor (url &rest args)
  "Ask the TOR browser to load URL."
  (interactive (browse-url-interactive-arg "URL: "))
  (let ((url (browse-url-encode-url url))
        (process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "tor " url) nil
	   "torbrowser"
	   (append args (list url)))))

(provide 'al-browse-url)

;;; al-browse-url.el ends here
