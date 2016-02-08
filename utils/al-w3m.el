;;; al-w3m.el --- Additional functionality for w3m

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 24 Sep 2013

;;; Code:

(require 'cl-lib)
(require 'w3m)
(require 'wget nil t)


;;; Go to the next/previous link

(defvar al/w3m-search-link-depth 10
  "The number of links to search for the next/previous URL.
See `al/w3m-next-url'/`al/w3m-previous-url' for details.")

(defvar al/w3m-search-re "\\<%s\\>"
  "Regexp for searching next/previous URL.
The string should contain \"%s\"-expression substituted by a
searched word. ")

(defun al/w3m-search-url (word point fun)
  "Search an URL anchor beginning with WORD.

POINT is the start point for searching.

FUN is a function used for going to an anchor (like
`w3m-next-anchor' or `w3m-previous-anchor').  FUN is called
`al/w3m-search-link-depth' times.

Return URL of the found anchor or nil if the link is not found."
  (save-excursion
    (goto-char point)
    (cl-loop for i from 1 to al/w3m-search-link-depth
             do (funcall fun)
             if (looking-at (format al/w3m-search-re word))
             return (w3m-anchor))))

(defmacro al/w3m-define-goto-url (type)
  "Define a function for going to the next/previous page.
TYPE should be a string \"next\" or \"previous\".
Defined function has a name `al/w3m-TYPE-url'."
  (let ((name (intern (concat "al/w3m-" type "-url")))
        (desc (concat "Go to the " type " page.\n"
                      "If `w3m-" type "-url' is nil, search in the first and last\n"
                      "`al/w3m-search-link-depth' links for the " type " page URL."))
        (type-url    (intern (concat "w3m-" type "-url"))))
    `(defun ,name ()
       ,desc
       (interactive)
       (let ((url (or ,type-url
                      (al/w3m-search-url ,type (point-min) 'w3m-next-anchor)
                      (al/w3m-search-url ,type (point-max) 'w3m-previous-anchor))))
         (if url
             (let ((w3m-prefer-cache t))
               (w3m-history-store-position)
               (w3m-goto-url url))
           (message ,(concat "No '" type "' link found.")))))))

(al/w3m-define-goto-url "next")
(al/w3m-define-goto-url "previous")

;;;###autoload (autoload 'al/w3m-next-url "al/w3m" nil t)
;;;###autoload (autoload 'al/w3m-previous-url "al/w3m" nil t)



;;;###autoload
(defun al/w3m-wget ()
  "Download anchor, image, or current page.
Same as `w3m-wget' but works."
  (interactive)
  (let ((url (or (w3m-anchor) (w3m-image)))
        (wget-current-title w3m-current-title))
    (wget-api url w3m-current-url)))

(defun al/w3m-buffer-number-action (function buffer-number)
  "Call FUNCTION on a w3m buffer with BUFFER-NUMBER.
Buffers are enumerated from 1."
  (let ((buf (nth (- arg 1) (w3m-list-buffers))))
    (and buf (funcall function buf))))

;;;###autoload
(defun al/w3m-switch-to-buffer (arg)
  "Switch to a w3m buffer number ARG.
Buffers are enumerated from 1."
  (interactive "NSwitch to w3m buffer number: ")
  (al/w3m-buffer-number-action #'switch-to-buffer arg))

;;;###autoload
(defun al/w3m-kill-buffer (arg)
  "Kill a w3m buffer number ARG.
Buffers are enumerated from 1."
  (interactive "NKill w3m buffer number: ")
  (al/w3m-buffer-number-action #'kill-buffer arg))

(defmacro al/w3m-bind-number-keys (fun &optional kbd-prefix)
  "Bind number keys (1-9) to a command that takes a numeric argument.
For example to bind <N> keys for switching to w3m buffers (tabs)
and to bind 'k <N>' keys for killing w3m buffers, use:

  (al/w3m-bind-number-keys 'al/w3m-switch-to-buffer)
  (al/w3m-bind-number-keys 'al/w3m-kill-buffer \"k\")

To bind the keys, `bind-key' function is used."
  (let ((numbers (number-sequence 1 9))
        (prefix (and kbd-prefix (concat kbd-prefix " "))))
    `(progn
       ,@(mapcar (lambda (n)
                   `(bind-key ,(concat prefix (number-to-string n))
                              (lambda () (interactive)
                                (funcall ,fun ,n))
                              w3m-mode-map))
                 numbers))))

(provide 'al-w3m)

;;; al-w3m.el ends here
