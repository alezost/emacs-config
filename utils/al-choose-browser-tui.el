;;; al-choose-browser-tui.el --- Transient interface for choosing a browser to open URL  -*- lexical-binding: t -*-

;; Copyright © 2025–2026 Alex Kost

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

;; This file provides `al/choose-browser' transient command that can
;; be used to choose a browser before opening an URL:
;;
;;   (setq browse-url-browser-function 'al/choose-browser-tui)

;;; Code:

(require 'transient)
(require 'browse-url)
(require 'al-browse-url)
(require 'misc)

(defun al/choose-browser-read-url (prompt _initial-input history)
  ;; Transient does not put the current value to INITIAL-INPUT ?!!
  ;; OK, than use the first value of history as the initial input.
  (let ((history (symbol-value history)))
    (completing-read prompt (cdr history) nil nil (car history))))

(defun al/choose-browser-args (&optional new-window-arg)
  "Return arguments for the current `al/choose-browser' transient."
  (let ((args (transient-args 'al/choose-browser)))
    (cons (transient-arg-value "url=" args)
          (and new-window-arg
               (list (transient-arg-value "--new-window" args))))))

(transient-define-argument al/choose-browser:url ()
  :description "URL"
  :class 'transient-option
  :key "U"
  :argument "url="
  :reader #'al/choose-browser-read-url
  :always-read t)

(transient-define-argument al/choose-browser:new-window ()
  :description "new window"
  :class 'transient-switch
  :key "n"
  :argument "--new-window")

(transient-define-suffix al/choose-browser-default (url new-window)
  (interactive (al/choose-browser-args t))
  (apply #'al/browse-url-default url
         (and new-window '("--new-window"))))

(transient-define-suffix al/choose-browser-tor (url new-window)
  (interactive (al/choose-browser-args t))
  (apply #'al/browse-url-tor url
         (and new-window '("--new-window"))))

(transient-define-suffix al/choose-browser-firefox (url new-window)
  (interactive (al/choose-browser-args t))
  (browse-url-firefox url new-window))

(transient-define-suffix al/choose-browser-chromium (url new-window)
  (interactive (al/choose-browser-args t))
  (browse-url-chromium url new-window))

(declare-function w3m-browse-url "w3m" (url))

(transient-define-suffix al/choose-browser-w3m (url)
  (interactive (al/choose-browser-args))
  (w3m-browse-url url))

(transient-define-suffix al/choose-browser-eww (url)
  (interactive (al/choose-browser-args))
  (eww url))

(transient-define-suffix al/choose-browser-emacs (url)
  (interactive (al/choose-browser-args))
  (browse-url-emacs url))

(transient-define-suffix al/choose-browser-kill-url (url)
  "Copy URL to `kill-ring' and clipboard."
  (interactive (al/choose-browser-args))
  (let ((select-enable-clipboard t)
        (select-enable-primary t))
    (al/with-eval-to-kill-ring url)))

;;;###autoload (autoload 'al/choose-browser "al-choose-browser-tui" nil t)
(transient-define-prefix al/choose-browser (url &rest _args)
  "Choose a browser to open URL.
Suitable for `browse-url-browser-function'."
  [(al/choose-browser:url)
   (al/choose-browser:new-window)]
  [("k" "kill URL" al/choose-browser-kill-url)]
  ["Browser"
   [:pad-keys t
    ("RET" "default"  al/choose-browser-default)
    ("u"   "default"  al/choose-browser-default)
    ("b"   "default"  al/choose-browser-default)]
   [("f"   "Firefox"  al/choose-browser-firefox)
    ("t"   "TOR Browser" al/choose-browser-tor)]
   [("c"   "Chromium" al/choose-browser-chromium)]
   [("w"   "w3m"      al/choose-browser-w3m)
    ("e"   "eww"      al/choose-browser-eww)
    ("E"   "Emacs"    al/choose-browser-emacs)]]
  (interactive "sURL: ")
  (transient-setup 'al/choose-browser nil nil
                   :value (list (concat "url=" url))))

(provide 'al-choose-browser-tui)

;;; al-choose-browser-tui.el ends here
