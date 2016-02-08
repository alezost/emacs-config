;;; utl-eshell.el --- Additional functionality for eshell

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 4 Sep 2013

;;; Code:

(require 'em-dirs)
(require 'em-unix)

(defun utl-eshell-kill-whole-line (arg)
  "Similar to `kill-whole-line', but respect eshell prompt."
  (interactive "p")
  (if (< (point) eshell-last-output-end)
      (kill-whole-line arg)
    (kill-region eshell-last-output-end
                 (progn (forward-line arg) (point)))))

;;;###autoload
(defun utl-eshell-cd (arg)
  "Start eshell and change directory there to the current one.
ARG has the same meaning as in `eshell'"
  (interactive "P")
  (let ((dir default-directory))
    (eshell arg)
    (eshell/cd dir)))

(declare-function Info-find-node "info" t)
(declare-function Info-menu "info" t)

(defun utl-eshell/info (&rest args)
  "Run `info' command on NAME.
NAME is the car of ARGS.

This function is intended to be used as a substitution for
`eshell/info'.  It does the following:

  info       =>  go to top info window;
  info NAME  =>  if NAME is a file '*.info', visit it;
  info NAME  =>  otherwise go to top info node and then menu item NAME."
  (require 'info)
  (let* ((name (car args))
         (file (and (stringp name)
                    (string-match "\\.info\\'" name)
                    (expand-file-name name))))
    (if (and file (file-exists-p file))
        (Info-find-node file "Top")
      (Info-directory)
      (Info-menu name))))


;;; Prompt

;; Idea from <http://www.emacswiki.org/emacs/EshellPrompt>.

;; TODO improve regexp
(defvar utl-eshell-prompt-regexp "^[#$] "
  "Regexp for `eshell-prompt-regexp'.")

(defmacro utl-with-face (str &rest properties)
  `(propertize ,str 'face (list ,@properties)))

(defun utl-eshell-prompt ()
  "Function for `eshell-prompt-function'."
  (format "%s %s%s%s %s\n%s "
          (utl-with-face (format-time-string "%H:%M" (current-time))
                        'font-lock-comment-face)
          (eshell/whoami)
          (utl-with-face "@"
                        'escape-glyph)
          system-name
          (utl-with-face (abbreviate-file-name (eshell/pwd))
                        'dired-directory)
          (utl-with-face (if (= (user-uid) 0) "#" "$")
                        'comint-highlight-prompt)))


;;; History

(require 'em-hist)

;;;###autoload
(defun utl-eshell-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
Unlike `eshell-previous-matching-input-from-input', the matching
input is not forced to begin with the current input."
  (interactive "p")
  (unless (memq last-command '(utl-eshell-previous-matching-input-from-input
                               utl-eshell-next-matching-input-from-input))
    ;; Starting a new search.
    (setq eshell-matching-input-from-input-string
          (buffer-substring (save-excursion (eshell-bol) (point))
                            (point))
          eshell-history-index nil))
  (eshell-previous-matching-input
   (regexp-quote eshell-matching-input-from-input-string)
   arg))

;;;###autoload
(defun utl-eshell-next-matching-input-from-input (arg)
  "Search forwards through input history for match for current input."
  (interactive "p")
  (utl-eshell-previous-matching-input-from-input (- arg)))

(provide 'utl-eshell)

;;; utl-eshell.el ends here
