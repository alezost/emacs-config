;;; al-browse-url.el --- Additional functionality for browse-url package  -*- lexical-binding: t -*-

;; Copyright © 2013–2025 Alex Kost

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

(require 'browse-url)
(require 'transient)


;;; Browse IRC logs from gnunet

(defvar al/irc-log-base-url "https://gnunet.org/bot/log/"
  "Base URL with IRC logs.")

(defvar al/irc-log-channels '("guix" "guile" "gnunet")
  "List of channels that are logged by gnunet bot.")

(declare-function url-expand-file-name "url-expand" t)
(declare-function org-read-date "org" t)

;;;###autoload
(defun al/browse-irc-log (channel &optional date)
  "Browse IRC log of the CHANNEL from DATE."
  (interactive
   (list (completing-read "IRC channel: " al/irc-log-channels nil t)
         (org-read-date nil nil nil "Log date: ")))
  (require 'url-expand)
  (browse-url (url-expand-file-name (concat channel "/" date)
                                    al/irc-log-base-url)))


;;; Additional browsers

(defcustom al/browse-url-program "browser"
  "Shell command name for the default browser."
  :type 'string
  :group 'browse-url)

(defcustom al/browse-url-arguments nil
  "A list of strings to pass to the default browser as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

;;;###autoload
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


;;; Transient interface to choose a browser

;; I use the following to be prompted for a browser before opening an URL:
;;
;;   (setq browse-url-browser-function 'al/choose-browser)

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
               (list (transient-arg-value "-new-window" args))))))

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
  :argument "-new-window")

(transient-define-suffix al/choose-browser-default (url new-window)
  (interactive (al/choose-browser-args t))
  (apply #'al/browse-url-default url
         (and new-window '("-new-window"))))

(transient-define-suffix al/choose-browser-firefox (url new-window)
  (interactive (al/choose-browser-args t))
  (apply #'browse-url-firefox url
         (and new-window '("-new-window"))))

(transient-define-suffix al/choose-browser-chromium (url)
  (interactive (al/choose-browser-args))
  (browse-url-chromium url))

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

;;;###autoload (autoload 'al/choose-browser "al-browse-url" nil t)
(transient-define-prefix al/choose-browser (url &rest _args)
  "Choose a browser to open URL.
Suitable for `browse-url-browser-function'."
  [(al/choose-browser:url)
   (al/choose-browser:new-window)]
  ["Browser"
   [:pad-keys t
    ("RET" "default"  al/choose-browser-default)
    ("u"   "default"  al/choose-browser-default)
    ("b"   "default"  al/choose-browser-default)]
   [("f"   "firefox"  al/choose-browser-firefox)]
   [("c"   "chromium" al/choose-browser-chromium)]
   [("w"   "w3m"      al/choose-browser-w3m)]
   [("e"   "eww"      al/choose-browser-eww)]
   [("E"   "emacs"    al/choose-browser-emacs)]]
  (interactive "sURL: ")
  (transient-setup 'al/choose-browser nil nil
                   :value (list (concat "url="url))))

(provide 'al-browse-url)

;;; al-browse-url.el ends here
