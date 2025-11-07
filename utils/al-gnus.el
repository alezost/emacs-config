;;; al-gnus.el --- Additional functionality for Gnus  -*- lexical-binding: t -*-

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

(require 'gnus-sum)
(require 'gnus-art)
(require 'al-buffer)

(defun al/gnus-buffer-p ()
  "Return nil if current buffer is not a gnus buffer."
  (memq (current-buffer) (gnus-buffers)))

;;;###autoload
(defun al/gnus-switch-to-group-buffer ()
  "Switch to gnus group buffer if it exists, otherwise start gnus."
  (interactive)
  (if (and (fboundp 'gnus-alive-p)
           (gnus-alive-p))
      (switch-to-buffer gnus-group-buffer)
    (gnus)))

;;;###autoload
(defun al/gnus-switch-buffer ()
  "Switch to a gnus buffer."
  (interactive)
  (let ((buffers (gnus-buffers)))
    (if buffers
     	(al/switch-buffer "Gnus buffer: "
                          :buffers buffers)
      (user-error "No Gnus buffers"))))

(defvar al/gnus-unbuttonized-mime-types-original
  gnus-unbuttonized-mime-types)

;;;###autoload
(defun al/gnus-summary-toggle-display-buttonized ()
  "Toggle the buttonizing of the article buffer."
  (interactive)
  (setq gnus-unbuttonized-mime-types
        (if (setq gnus-inhibit-mime-unbuttonizing
                  (not gnus-inhibit-mime-unbuttonizing))
            al/gnus-unbuttonized-mime-types-original
          '(".*/.*")))
  (gnus-summary-show-article))


;;; Switching gnus and non-gnus window configurations

;; Idea from <http://www.emacswiki.org/emacs/SwitchToGnus>.

(defvar al/gnus-win-config nil
  "Window configuration with gnus buffers.")

(defvar al/non-gnus-win-config nil
  "Window configuration with non-gnus buffers.")

(defun al/gnus-win-config-variable (&optional revert)
  "Return a name of variable with window configuration.
Return `al/gnus-win-config' if current buffer is a gnus buffer,
return `al/non-gnus-win-config' otherwise.
If REVERT is non-nil, do vice versa (return the other variable)."
  (if (xor (al/gnus-buffer-p) revert)
      'al/gnus-win-config
    'al/non-gnus-win-config))

(defun al/gnus-save-win-config ()
  "Save current gnus or non-gnus window configuration."
  (interactive)
  (set (al/gnus-win-config-variable)
       (current-window-configuration)))

;;;###autoload
(defun al/gnus-switch-win-config ()
  "Switch window configuration between gnus and non-gnus buffers.
Start Gnus if needed."
  (interactive)
  (al/gnus-save-win-config)
  (if (gnus-alive-p)
      (set-window-configuration
       (symbol-value (al/gnus-win-config-variable 'other)))
    (gnus)
    (al/gnus-save-win-config)))


;;; Finding URLs in summary and article buffers

(defvar al/gnus-link-re "\\<link\\>"
  "Regexp matching a link name.
Used in `al/gnus-summary-find-link-url'.")

(defvar al/gnus-mm-url-re "\\.mp3$"
  "Regexp for multimedia links.
Used in `al/gnus-summary-find-mm-url'.")

(defun al/widget-next ()
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

(defun al/gnus-article-find-url (predicate)
  "Return the first widget URL matching PREDICATE.
Return nil if no matches found."
  (save-excursion
    (article-goto-body)
    (backward-char)
    (al/gnus-article-find-url-1 predicate)))

(defun al/gnus-article-find-url-1 (predicate)
  (al/widget-next)
  (unless (eobp)
    (let* ((point (point))
           ;; Text property with URL depends on `mm-text-html-renderer'.
           (url (or (get-text-property point 'gnus-string)
                    (get-text-property point 'shr-url))))
      (if (and url (funcall predicate url))
          url
        (al/gnus-article-find-url-1 predicate)))))

(defun al/gnus-article-find-url-by-re (regexp &optional group)
  "Return the first widget URL matching REGEXP.
If GROUP is non-nil, it should be a number specifying a
parenthesized expression from REGEXP that should be returned.
Return nil if no matches found."
  (let ((url (al/gnus-article-find-url
              (lambda (url) (string-match-p regexp url)))))
    (if (null group)
        url
      (string-match regexp url)
      (match-string group url))))

(defun al/gnus-article-find-url-by-name (regexp)
  "Return the first widget URL with widget name matching REGEXP.
Return nil if no matches found."
  (al/gnus-article-find-url
   (lambda (_) (looking-at regexp))))

(defmacro al/gnus-summary-eval-in-article (&rest body)
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

(defun al/gnus-summary-find-url-by-re (regexp &optional group)
  "Return the first URL from the gnus article matching REGEXP.
See `al/gnus-article-find-url-by-re' for details."
  (al/gnus-summary-eval-in-article
   (al/gnus-article-find-url-by-re regexp group)))

(defun al/gnus-summary-find-url-by-name (regexp)
  "Return the first URL from the gnus article with name matching REGEXP.
See `al/gnus-article-find-url-by-name' for details."
  (al/gnus-summary-eval-in-article
   (al/gnus-article-find-url-by-name regexp)))

(defun al/gnus-summary-find-link-url ()
  "Return the first \"link\" URL from the gnus article.
Matching url is defined by `al/gnus-link-re'."
  (al/gnus-summary-find-url-by-name al/gnus-link-re))

(defun al/gnus-summary-find-mm-url ()
  "Return the first multimedia URL from the gnus article.
Matching url is defined by `al/gnus-mm-url-re'."
  (al/gnus-summary-find-url-by-re al/gnus-mm-url-re))

;;;###autoload
(defun al/gnus-summary-browse-link-url ()
  "Browse the first \"link\" URL from the gnus article."
  (interactive)
  (browse-url (al/gnus-summary-find-link-url)))

(declare-function emms-add-url "emms-source-file" (url))
(declare-function emms-play-url "emms-source-file" (url))

;;;###autoload
(defun al/gnus-summary-emms-add-url ()
  "Add the first multimedia URL from gnus article to EMMS playlist."
  (interactive)
  (emms-add-url (al/gnus-summary-find-mm-url)))

;;;###autoload
(defun al/gnus-summary-emms-play-url ()
  "Play the first multimedia URL from gnus article with EMMS."
  (interactive)
  (emms-play-url (al/gnus-summary-find-mm-url)))


;;; Convert Atom to RSS

;; The code for `al/convert-atom-to-rss' is taken from a defadvice from
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

(defvar al/atom2rss-file
  (expand-file-name "atom2rss.xsl" user-emacs-directory)
  "Path to \"atom2rss.xsl\" file for converting Atom to RSS.")

(defun al/convert-atom-to-rss (&rest _)
  "Convert Atom to RSS (if needed) by calling xsltproc.
This function is intendend to be used as an `after' advice for
`mm-url-insert'."
  (when (re-search-forward "xmlns=\"http://www.w3.org/.*/Atom\""
			   nil t)
    (goto-char (point-min))
    (message "Converting Atom to RSS... ")
    (call-process-region (point-min) (point-max)
			 "xsltproc"
			 t t nil
			 al/atom2rss-file "-")
    (goto-char (point-min))
    (message "Converting Atom to RSS... done")))


;;; Agent mode-line string

(defvar al/gnus-plugged " ↔"
  "Mode-line string indicating that Gnus is plugged.
Used by `al/change-mode-string' advice for
`gnus-agent-make-mode-line-string'.")

(defvar al/gnus-unplugged " ↮"
  "Mode-line string indicating that Gnus is unplugged.
Used by `al/change-mode-string' advice for
`gnus-agent-make-mode-line-string'.")

(defun al/gnus-plugged-status (string)
  "Return `al/gnus-plugged' or `al/gnus-unplugged' depending on STRING."
  (cond
   ((string= string " Plugged") al/gnus-plugged)
   ((string= string " Unplugged") al/gnus-unplugged)
   (t " unknown")))

(defun al/gnus-agent-mode-line-string (fun string &rest args)
  "Modify \"Plugged\"/\"Unplugged\" mode-line string.
This function is intendend to be used as an `around' advice for
`gnus-agent-make-mode-line-string'."
  (apply fun (al/gnus-plugged-status string) args))


;;; Miscellaneous

;;;###autoload
(defun al/gnus-group-next-unread-group (n)
  "Go to next N'th unread newsgroup.
This is the same as `gnus-group-next-unread-group' except it
doesn't honor `gnus-group-goto-unread'."
  (interactive "p")
  (let ((gnus-group-goto-unread t))
    (gnus-group-next-unread-group n)))

;;;###autoload
(defun al/gnus-group-prev-unread-group (n)
  "Go to previous N'th unread newsgroup.
This is the same as `gnus-group-prev-unread-group' except it
doesn't honor `gnus-group-goto-unread'."
  (interactive "p")
  (al/gnus-group-next-unread-group (- n)))

(provide 'al-gnus)

;;; al-gnus.el ends here
