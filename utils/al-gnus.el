;;; al-gnus.el --- Additional functionality for Gnus   -*- lexical-binding: t -*-

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 30 Jul 2013

;;; Code:

(require 'gnus-sum)
(require 'gnus-art)
(require 'al-misc)  ; for al/xor

(defun utl-gnus-buffer-names ()
  "Return a list of names of live gnus buffer."
  (mapcar #'buffer-name (gnus-buffers)))

(defun utl-gnus-buffer-p ()
  "Return nil if current buffer is not a gnus buffer."
  (memq (current-buffer) (gnus-buffers)))

;;;###autoload
(defun utl-gnus-switch-to-group-buffer ()
  "Switch to gnus group buffer if it exists, otherwise start gnus."
  (interactive)
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (switch-to-buffer gnus-group-buffer)
    (gnus)))

;;;###autoload
(defun utl-gnus-ido-switch-buffer ()
  "Switch to gnus buffer, or start gnus if not already started.
Gnus buffer is selected using IDO."
  (interactive)
  (let ((gnus-bufs (utl-gnus-buffer-names)))
    (if gnus-bufs
     	(switch-to-buffer (completing-read "Gnus buffer: " gnus-bufs))
      (gnus))))


;;; Switching gnus and non-gnus window configurations

;; Idea from <http://www.emacswiki.org/emacs/SwitchToGnus>.

(defvar utl-gnus-win-config nil
  "Window configuration with gnus buffers.")

(defvar utl-non-gnus-win-config nil
  "Window configuration with non-gnus buffers.")

(defun utl-gnus-win-config-variable (&optional revert)
  "Return a name of variable with window configuration.
Return `utl-gnus-win-config' if current buffer is a gnus buffer,
return `utl-non-gnus-win-config' otherwise.
If REVERT is non-nil, do vice versa (return the other variable)."
  (if (utl-xor (utl-gnus-buffer-p) revert)
      'utl-gnus-win-config
    'utl-non-gnus-win-config))

(defun utl-gnus-save-win-config ()
  "Save current gnus or non-gnus window configuration."
  (interactive)
  (set (utl-gnus-win-config-variable)
       (current-window-configuration)))

;;;###autoload
(defun utl-gnus-switch-win-config ()
  "Switch window configuration between gnus and non-gnus buffers.
Start Gnus if needed."
  (interactive)
  (utl-gnus-save-win-config)
  (if (gnus-alive-p)
      (set-window-configuration
       (symbol-value (utl-gnus-win-config-variable 'other)))
    (gnus)
    (utl-gnus-save-win-config)))


;;; Finding URLs in summary and article buffers

(defvar utl-gnus-link-re "\\<link\\>"
  "Regexp matching a link name.
Used in `utl-gnus-summary-find-link-url'.")

(defvar utl-gnus-mm-url-re "\\.mp3$"
  "Regexp for multimedia links.
Used in `utl-gnus-summary-find-mm-url'.")

(defun utl-widget-next ()
  "Move point to the next field or button.
After the last widget, move point to the end of buffer."
  ;; The code is a rework of `widget-move'.
  (let ((old (widget-tabable-at))
        (move (if widget-use-overlay-change
                  (lambda () (goto-char (next-overlay-change (point))))
                (lambda () (forward-char 1)))))
    (funcall move)
    (while (let ((new (widget-tabable-at)))
             (and (or (null new) (eq new old))
                  (not (eobp))))
      (funcall move))))

(defun utl-gnus-article-find-url (predicate)
  "Return the first widget URL matching PREDICATE.
Return nil if no matches found."
  (save-excursion
    (article-goto-body)
    (backward-char)
    (utl-gnus-article-find-url-1 predicate)))

(defun utl-gnus-article-find-url-1 (predicate)
  (utl-widget-next)
  (unless (eobp)
    (let* ((point (point))
           ;; Text property with URL depends on `mm-text-html-renderer'.
           (url (or (get-text-property point 'gnus-string)
                    (get-text-property point 'shr-url))))
      (if (and url (funcall predicate url))
          url
        (utl-gnus-article-find-url-1 predicate)))))

(defun utl-gnus-article-find-url-by-re (regexp &optional group)
  "Return the first widget URL matching REGEXP.
If GROUP is non-nil, it should be a number specifying a
parenthesized expression from REGEXP that should be returned.
Return nil if no matches found."
  (let ((url (utl-gnus-article-find-url
              (lambda (url) (string-match-p regexp url)))))
    (if (null group)
        url
      (string-match regexp url)
      (match-string group url))))

(defun utl-gnus-article-find-url-by-name (regexp)
  "Return the first widget URL with widget name matching REGEXP.
Return nil if no matches found."
  (utl-gnus-article-find-url
   (lambda (_) (looking-at regexp))))

(defmacro utl-gnus-summary-eval-in-article (&rest body)
  "Display an article buffer and evaluate BODY there."
  ;; The code is taken from `gnus-summary-next-page'.
  `(let ((article (gnus-summary-article-number)))
     (or article
         (error "No article to select"))
     (gnus-configure-windows 'article)
     ;; Selected subject is different from the current article's subject.
     (if (or (null gnus-current-article)
             (null gnus-article-current)
             (/= article (cdr gnus-article-current))
             (not (equal (car gnus-article-current) gnus-newsgroup-name)))
         (gnus-summary-display-article article))
     (gnus-eval-in-buffer-window gnus-article-buffer
       ,@body)))

(defun utl-gnus-summary-find-url-by-re (regexp &optional group)
  "Return the first URL from the gnus article matching REGEXP.
See `utl-gnus-article-find-url-by-re' for details."
  (utl-gnus-summary-eval-in-article
   (utl-gnus-article-find-url-by-re regexp group)))

(defun utl-gnus-summary-find-url-by-name (regexp)
  "Return the first URL from the gnus article with name matching REGEXP.
See `utl-gnus-article-find-url-by-name' for details."
  (utl-gnus-summary-eval-in-article
   (utl-gnus-article-find-url-by-name regexp)))

(defun utl-gnus-summary-find-link-url ()
  "Return the first \"link\" URL from the gnus article.
Matching url is defined by `utl-gnus-link-re'."
  (utl-gnus-summary-find-url-by-name utl-gnus-link-re))

(defun utl-gnus-summary-find-mm-url ()
  "Return the first multimedia URL from the gnus article.
Matching url is defined by `utl-gnus-mm-url-re'."
  (utl-gnus-summary-find-url-by-re utl-gnus-mm-url-re))

;;;###autoload
(defun utl-gnus-summary-browse-link-url ()
  "Browse the first \"link\" URL from the gnus article."
  (interactive)
  (browse-url (utl-gnus-summary-find-link-url)))

(declare-function emms-add-url "emms-source-file" (url))
(declare-function emms-play-url "emms-source-file" (url))

;;;###autoload
(defun utl-gnus-summary-emms-add-url ()
  "Add the first multimedia URL from gnus article to EMMS playlist."
  (interactive)
  (emms-add-url (utl-gnus-summary-find-mm-url)))

;;;###autoload
(defun utl-gnus-summary-emms-play-url ()
  "Play the first multimedia URL from gnus article with EMMS."
  (interactive)
  (emms-play-url (utl-gnus-summary-find-mm-url)))


;;; Convert Atom to RSS

;; The code for `utl-convert-atom-to-rss' is taken from a defadvice from
;; <http://www.emacswiki.org/emacs/GnusRss>.  The original
;; "atom2rss.xsl" is taken from <http://atom.geekhood.net/>.

;; Github private feed (with info from <https://github.com>) is an Atom,
;; so we need to convert it to use with gnus.  There is a little
;; problem: "atom2rss.xsl" tries to insert a comment with self link to
;; the resulting rss, but a github private link may contain "--" in it
;; (for me this link is:
;; "https://github.com/alezost.private.atom?token=a_lot_of_numbers_and_letters--more_numers_and_letters")
;; and as it is not allowed in xml comments, xsltproc throws an error.

;; To fix that, I commented the line:
;;
;;   <x:template match="atom:feed/atom:link[@rel='self']"> ...
;;
;; in "atom2rss.xsl" and now I can check github feed in gnus. Hooray!

(defvar utl-atom2rss-file
  (expand-file-name "atom2rss.xsl" user-emacs-directory)
  "Path to \"atom2rss.xsl\" file for converting Atom to RSS.")

(defun utl-convert-atom-to-rss (&rest _)
  "Convert Atom to RSS (if needed) by calling xsltproc.
This function is intendend to be used as an 'after' advice for
`mm-url-insert', i.e.:

  (advice-add 'mm-url-insert :after #'utl-convert-atom-to-rss)"
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max)
			 "xsltproc"
			 t t nil
			 utl-atom2rss-file "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))


;;; Agent mode-line string

(defvar utl-gnus-plugged " ↔"
  "Mode-line string indicating that Gnus is plugged.
Used by `utl-change-mode-string' advice for
`gnus-agent-make-mode-line-string'.")

(defvar utl-gnus-unplugged " ↮"
  "Mode-line string indicating that Gnus is unplugged.
Used by `utl-change-mode-string' advice for
`gnus-agent-make-mode-line-string'.")

(defun utl-gnus-plugged-status (string)
  "Return `utl-gnus-plugged' or `utl-gnus-unplugged' depending on STRING."
  (cond
   ((string= string " Plugged") utl-gnus-plugged)
   ((string= string " Unplugged") utl-gnus-unplugged)
   (t " unknown")))

(defun utl-gnus-agent-mode-line-string (fun string &rest args)
  "Modify \"Plugged\"/\"Unplugged\" mode-line string.
This function is intendend to be used as an 'around' advice for
`gnus-agent-make-mode-line-string', i.e.:

  (advice-add 'gnus-agent-make-mode-line-string
              :around #'utl-gnus-agent-mode-line-string)"
  (apply fun (utl-gnus-plugged-status string) args))

(provide 'al-gnus)

;;; al-gnus.el ends here
