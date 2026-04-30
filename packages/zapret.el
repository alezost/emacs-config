;;; zapret.el --- Major mode for files with "zapret" command line options  -*- lexical-binding: t -*-

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

;;; Commentary:

;; This file provides `zapret-nfqws-mode', a major mode for files with
;; command line options for "nfqws" <https://github.com/bol-van/zapret/>
;; and "nfqws2" <https://github.com/bol-van/zapret2/>.

;;; Code:

(require 'bui-button)

(defgroup zapret nil
  "Tools for editing \"zapret\" command line files."
  :group 'data)

(defgroup zapret-faces nil
  "Faces for \"zapret\" command line files."
  :group 'data)

(defface zapret-colon
  '((t :background "dark gray"
       :foreground "black"))
  "Face for \":\" symbol."
  :group 'zapret-faces)

(defface zapret-new
  '((t :inherit font-lock-keyword-face))
  "Face for \"--new\" option."
  :group 'zapret-faces)

(defface zapret-name
  '((t :inherit font-lock-type-face))
  "Face for \"--name\" option."
  :group 'zapret-faces)

(defface zapret-comment
  '((t :inherit font-lock-comment-face))
  "Face for \"--comment\" option."
  :group 'zapret-faces)

(defface zapret-option
  '((t :inherit font-lock-variable-name-face))
  "Face for most command line options."
  :group 'zapret-faces)

(defface zapret-file-link
  '((t :inherit link))
  "Face for file links."
  :group 'zapret-faces)

(defvar zapret-font-lock-keywords
  '(("--new" . 'zapret-new)
    ("--name" . 'zapret-name)
    (":" . 'zapret-colon)
    ("--comment=[^ \n\t]*" 0 'zapret-comment t)
    ("\\(--[[:alnum:]-]+\\)=" 1 'zapret-option)
    zapret-fontify-links)
  "`font-lock-keywords' for `zapret-nfqws-mode'.")

(defun zapret-fontify-link (start end)
  "Add link properties to text from START point position to END."
  (make-text-button
   start end
   :type 'bui-file
   'face 'zapret-file-link))

(defun zapret-fontify-links (limit)
  "Add link properties to file names."
  (when (re-search-forward "/[^ \n\t]+" limit t)
    (zapret-fontify-link (match-beginning 0)
                         (match-end 0))
    t))

;;;###autoload
(define-derived-mode zapret-nfqws-mode prog-mode "zapret"
  "Major mode for \"nfqws\" and \"nfqws2\" command line options."
  (setq-local comment-start "--comment="
              comment-padding "")
  (setq font-lock-defaults (list zapret-font-lock-keywords)))

(provide 'zapret)

;;; zapret.el ends here
