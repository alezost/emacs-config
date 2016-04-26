;;; al-browse-url.el --- Additional functionality for browse-url package

;; Copyright © 2013-2016 Alex Kost

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

;;; Code:

(require 'browse-url)
(require 'cl-lib)


;;; Browse IRC logs from gnunet.

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


;;; Add support for the Conkeror browser.

(defcustom al/browse-url-conkeror-program "conkeror"
  "The name by which to invoke Conkeror."
  :type 'string
  :group 'browse-url)

(defcustom al/browse-url-conkeror-arguments nil
  "A list of strings to pass to Conkeror as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

;;;###autoload
(defun al/browse-url-conkeror (url &optional new-window)
  "Ask the Conkeror WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `al/browse-url-conkeror-arguments' are also passed to
Conkeror."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "conkeror " url) nil
	   al/browse-url-conkeror-program
	   (append al/browse-url-conkeror-arguments
                   (list url)))))


;;; Choosing a browser

;; I use the following to be prompted for a browser before opening an URL:
;;
;;   (setq browse-url-browser-function 'al/choose-browser)

(defvar al/browser-choices
  '(((?c ?\C-m) "conkeror" al/browse-url-conkeror)
    (?f "firefox" browse-url-firefox)
    (?w "w3m" w3m-browse-url)
    (?e "eww" eww))
  "List of the browser choices for `al/choose-browser'.
Each choice has a form:

  (CHAR NAME FUN)

CHAR is a character or a list of characters that can be pressed.
NAME is a name of the browser.
FUN is a function to call for browsing (should take URL as an argument).

The first choice is used as default (pressing RET will call the
first function).")

;;;###autoload
(defun al/choose-browser (url &rest args)
  "Choose a browser for openning URL.
Suitable for `browse-url-browser-function'."
  (interactive "sURL: ")
  (let* ((choices (mapcar
                   (lambda (spec)
                     (let* ((chars (car spec))
                            (chars (if (listp chars) chars (list chars)))
                            (name (cadr spec)))
                       (list chars name)))
                   al/browser-choices))
         (chars (cons ?\C-g
                      (apply #'append (mapcar #'car choices))))
         (str (mapconcat
               (lambda (spec)
                 (let ((chars (car spec))
                       (name  (cadr spec)))
                   (format "%s (%s)"
                           (mapconcat
                            (lambda (char)
                              (propertize (string char)
                                          'face 'font-lock-warning-face))
                            chars
                            "/")
                           name)))
               choices
               ", "))
         (char (read-char-choice
                (concat (propertize "Choose a browser for '"
                                    'face 'default)
                        url "'\n" str ": ")
                chars t)))
    (unless (eq char ?\C-g)
      (funcall (nth 2 (cl-find-if
                       (lambda (spec)
                         (let ((chars (car spec)))
                           (if (listp chars)
                               (memq char chars)
                             (eq char chars))))
                       al/browser-choices))
               url))
    (message "")))

(provide 'al-browse-url)

;;; al-browse-url.el ends here
