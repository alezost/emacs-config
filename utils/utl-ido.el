;;; utl-ido.el --- Additional functionality for ido-mode

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 18 May 2013

;;; Code:

(require 'ido)


;;; Use ido everywhere

;; Idea from <http://www.emacswiki.org/emacs/InteractivelyDoThings#toc15>.

;; To activate the following code, add to .emacs:
;;
;;   (setq completing-read-function 'utl-completing-read)

(defvar utl-ido-enable-replace-completing-read t
  "If non-nil, use `ido-completing-read' instead of `completing-read'.")

(defun utl-completing-read (prompt collection &optional predicate
                                   require-match initial-input
                                   hist def inherit-input-method)
  "Function for `completing-read-function' variable.
Use `ido-completing-read' if possible."
  (let (choices)
    (if (and utl-ido-enable-replace-completing-read
             (setq choices (all-completions "" collection predicate)))
        ;; Match is never required because with requiring it's
        ;; not possible to select "#XXXXXX" with `read-color'.
        (ido-completing-read prompt choices nil nil
                             initial-input hist def)
      (completing-read-default prompt collection predicate
                               require-match initial-input
                               hist def inherit-input-method))))

(defun utl-ido-disable (fun &rest args)
  "Disable `ido-completing-read' for FUN.
This function is intended to be used as an 'around' advice for
FUN, for example:

  (advice-add 'org-set-tags :around #'utl-ido-disable)"
  (let (utl-ido-enable-replace-completing-read)
    (apply fun args)))


(defvar ido-rotate-temp)

;;;###autoload
(defun utl-ido-set-current-directory (dir)
  "Change the current ido working directory to DIR."
  (interactive)
  (ido-set-current-directory dir)
  (setq ido-exit 'refresh)
  (setq ido-text-init ido-text)
  (setq ido-rotate-temp t)
  (exit-minibuffer))

;;;###autoload
(defun utl-ido-copy-current-item ()
  "Put the current ido item into `kill-ring'."
  (interactive)
  (kill-new (car ido-matches)))

(provide 'utl-ido)

;;; utl-ido.el ends here
