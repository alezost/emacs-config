;;; al-browse-url.el --- Additional functionality for browse-url package

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 24 Sep 2013

;;; Code:

(require 'browse-url)
(require 'cl-lib)


;;; Browse IRC logs from gnunet.

(defvar utl-irc-log-base-url "https://gnunet.org/bot/log/"
  "Base URL with IRC logs.")

(defvar utl-irc-log-channels '("guix" "guile" "gnunet")
  "List of channels that are logged by gnunet bot.")

(declare-function url-expand-file-name "url-expand" t)
(declare-function org-read-date "org" t)

;;;###autoload
(defun utl-browse-irc-log (channel &optional date)
  "Browse IRC log of the CHANNEL from DATE."
  (interactive
   (list (completing-read "IRC channel: " utl-irc-log-channels nil t)
         (org-read-date nil nil nil "Log date: ")))
  (require 'url-expand)
  (browse-url (url-expand-file-name (concat channel "/" date)
                                    utl-irc-log-base-url)))


;;; Add support for the Conkeror browser.

(defcustom utl-browse-url-conkeror-program "conkeror"
  "The name by which to invoke Conkeror."
  :type 'string
  :group 'browse-url)

(defcustom utl-browse-url-conkeror-arguments nil
  "A list of strings to pass to Conkeror as arguments."
  :type '(repeat (string :tag "Argument"))
  :group 'browse-url)

;;;###autoload
(defun utl-browse-url-conkeror (url &optional new-window)
  "Ask the Conkeror WWW browser to load URL.
Default to the URL around or before point.  The strings in
variable `utl-browse-url-conkeror-arguments' are also passed to
Conkeror."
  (interactive (browse-url-interactive-arg "URL: "))
  (setq url (browse-url-encode-url url))
  (let* ((process-environment (browse-url-process-environment)))
    (apply #'start-process
	   (concat "conkeror " url) nil
	   utl-browse-url-conkeror-program
	   (append utl-browse-url-conkeror-arguments
                   (list url)))))


;;; Choosing a browser

;; I use the following to be prompted for a browser before opening an URL:
;;
;;   (setq browse-url-browser-function 'utl-choose-browser)

(defvar utl-browser-choices
  '(((?c ?\C-m) "conkeror" utl-browse-url-conkeror)
    (?f "firefox" browse-url-firefox)
    (?w "w3m" w3m-browse-url)
    (?e "eww" eww))
  "List of the browser choices for `utl-choose-browser'.
Each choice has a form:

  (CHAR NAME FUN)

CHAR is a character or a list of characters that can be pressed.
NAME is a name of the browser.
FUN is a function to call for browsing (should take URL as an argument).

The first choice is used as default (pressing RET will call the
first function).")

;;;###autoload
(defun utl-choose-browser (url &rest args)
  "Choose a browser for openning URL.
Suitable for `browse-url-browser-function'."
  (interactive "sURL: ")
  (let* ((choices (mapcar
                   (lambda (spec)
                     (let* ((chars (car spec))
                            (chars (if (listp chars) chars (list chars)))
                            (name (cadr spec)))
                       (list chars name)))
                   utl-browser-choices))
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
                       utl-browser-choices))
               url))
    (message "")))

(provide 'al-browse-url)

;;; al-browse-url.el ends here
