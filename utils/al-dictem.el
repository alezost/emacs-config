;;; al-dictem.el --- Additional functionality for dictem

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 10 Mar 2013

;;; Code:

(require 'dictem)
(require 'al-misc)

(defun utl-dictem ()
  "Same as `dictem' but do not use other window for a dictem buffer."
  (let ((buffer (generate-new-buffer dictem-buffer-name))
	(window-configuration (current-window-configuration))
	(selected-window (frame-selected-window)))
    (switch-to-buffer buffer)      ; not `switch-to-buffer-other-window'
    (dictem-mode)
    (make-local-variable 'dictem-window-configuration)
    (make-local-variable 'dictem-selected-window)
    (make-local-variable 'dictem-content-history)
    (setq dictem-window-configuration window-configuration)
    (setq dictem-selected-window selected-window)))

(defun utl-dictem-define-on-press ()
  "Same as `dictem-define-on-press' but with \"*\" for dicts."
  (interactive)
  (let* ((properties (text-properties-at (point)))
	 (data (plist-get properties 'link-data))
	 (fun  (plist-get properties 'link-function))
	 (dictem-server (plist-get properties 'dictem-server))
	 (dictem-port   (plist-get properties 'dictem-port))
	 (word   (assq 'word data))
	 (dbname (assq 'dbname data)))
    (if (or word dbname)
	(dictem-run fun "*"
		    ;; "*" instead of:
                    ;; (if dbname (cdr dbname) dictem-last-database)
		    (if word (cdr word) nil)
		    nil))))

(defun utl-dictem-read-query (&optional default-query)
  "Similar to `dictem-read-query', but uses `utl-read-string'."
  (utl-read-string "Query word" nil 'dictem-query-history default-query))

(defvar utl-dictem-dicts '(nil "mueller7")
  "List of dictionaries to search by `utl-dictem-run-word'.
dictem ignores the first dictionary, so the first element of the
list should be nil.")

;;;###autoload
(defun utl-dictem-run-word (&optional word)
  "Ask about word and show definitions in new buffer."
  (interactive)
  (let ((dictem-server nil))
    (dictem-run 'dictem-base-define
                utl-dictem-dicts
                (or word (utl-dictem-read-query (thing-at-point 'word)))
                "exact")))

;;;###autoload
(defun utl-dictem-run-dict-search ()
  "Search in \"dict.org\". Ask about search strategy and query."
  (interactive)
  (let ((dictem-server "dict.org")
        (query (utl-dictem-read-query (thing-at-point 'word)))
        (strat (dictem-select-strategy (dictem-get-default-strategy))))
    (dictem-run 'dictem-base-search "*" query strat)))

;;;###autoload
(defun utl-dictem-run-show-all-info ()
  "Show information about all local databases"
  (interactive)
  (let ((dictem-server "localhost"))
    (dictem-run
     (lambda (a b c)
       (dictem-base-show-strategies nil nil nil)
       (dictem-base-show-databases nil nil nil)
       (dictem-base-show-server nil nil nil)))))

(provide 'al-dictem)

;;; al-dictem.el ends here
