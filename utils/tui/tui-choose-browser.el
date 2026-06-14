;;; tui-choose-browser.el --- Transient interface for choosing a browser to open URL  -*- lexical-binding: t -*-

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

;; This file provides `tui/choose-browser' command that can
;; be used to choose a browser before opening an URL:
;;
;;   (setq browse-url-browser-function 'tui/choose-browser)

;;; Code:

(require 'transient)
(require 'browse-url)
(require 'al-general)
(require 'al-browse-url)

(defun tui/choose-browser-read-url (prompt _initial-input history)
  ;; Transient does not put the current value to INITIAL-INPUT ?!!
  ;; OK, than use the first value of history as the initial input.
  (let ((history (symbol-value history)))
    (completing-read prompt (cdr history) nil nil (car history))))

(defun tui/choose-browser-args (&optional new-window-arg)
  "Return arguments for the current `tui/choose-browser' transient."
  (let ((args (transient-args 'tui/choose-browser)))
    (cons (transient-arg-value "url=" args)
          (and new-window-arg
               (list (transient-arg-value "--new-window" args))))))

(transient-define-argument tui/choose-browser:url ()
  :description "URL"
  :class 'transient-option
  :key "U"
  :argument "url="
  :reader #'tui/choose-browser-read-url
  :always-read t)

(transient-define-argument tui/choose-browser:new-window ()
  :description "new window"
  :class 'transient-switch
  :key "n"
  :argument "--new-window")

(transient-define-suffix tui/choose-browser:default (url new-window)
  (interactive (tui/choose-browser-args t))
  (apply #'al/browse-url-default url
         (and new-window '("--new-window"))))

(transient-define-suffix tui/choose-browser:tor (url new-window)
  (interactive (tui/choose-browser-args t))
  (apply #'al/browse-url-tor url
         (and new-window '("--new-window"))))

(transient-define-suffix tui/choose-browser:firefox (url new-window)
  (interactive (tui/choose-browser-args t))
  (apply #'al/browse-url-firefox url
         (and new-window '("--new-window"))))

(transient-define-suffix tui/choose-browser:firefox-profile (url new-window)
  (interactive (tui/choose-browser-args t))
  (let ((profile (completing-read "Profile: " (al/firefox-profiles))))
    (apply #'al/browse-url-firefox url
           "-P" profile
           (and new-window '("--new-window")))))

(transient-define-suffix tui/choose-browser:chromium (url new-window)
  (interactive (tui/choose-browser-args t))
  (browse-url-chromium url new-window))

(declare-function w3m-browse-url "w3m" (url))

(transient-define-suffix tui/choose-browser:w3m (url)
  (interactive (tui/choose-browser-args))
  (w3m-browse-url url))

(transient-define-suffix tui/choose-browser:eww (url)
  (interactive (tui/choose-browser-args))
  (eww url))

(transient-define-suffix tui/choose-browser:emacs (url)
  (interactive (tui/choose-browser-args))
  (browse-url-emacs url))

(transient-define-suffix tui/choose-browser:kill-url (url)
  "Copy URL to `kill-ring' and clipboard."
  (interactive (tui/choose-browser-args))
  (let ((select-enable-clipboard t)
        (select-enable-primary t))
    (al/eval-to-kill-ring url)))

;;;###autoload (autoload 'tui/choose-browser "tui-choose-browser" nil t)
(transient-define-prefix tui/choose-browser (url &rest _args)
  "Choose a browser to open URL.
Suitable for `browse-url-browser-function'."
  [(tui/choose-browser:url)
   (tui/choose-browser:new-window)]
  [("k"    "kill URL"           tui/choose-browser:kill-url)]
  ["Browser"
   [:pad-keys t
    ("RET" "default"            tui/choose-browser:default)
    ("u"   "default"            tui/choose-browser:default)
    ("b"   "default"            tui/choose-browser:default)]
   [("f"   "Firefox"            tui/choose-browser:firefox)
    ("F"   "Firefox profile"    tui/choose-browser:firefox-profile)
    ("t"   "TOR Browser"        tui/choose-browser:tor)]
   [("c"   "Chromium"           tui/choose-browser:chromium)]
   [("w"   "w3m"                tui/choose-browser:w3m)
    ("e"   "eww"                tui/choose-browser:eww)
    ("E"   "Emacs"              tui/choose-browser:emacs)]]
  (interactive "sURL: ")
  (transient-setup 'tui/choose-browser nil nil
                   :value (list (concat "url=" url))))

(provide 'tui-choose-browser)

;;; tui-choose-browser.el ends here
