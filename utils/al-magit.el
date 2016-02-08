;;; al-magit.el --- Additional functionality for magit

;; Author: Alex Kost <alezost@gmail.com>
;; Created:  9 Mar 2015

;;; Code:

(require 'cl-lib)
(require 'ido)

(defun utl-magit-popup-substitute-key (var-name property old-char new-char)
  "Change key binding of action/switch/option of a magit popup VAR-NAME.

PROPERTY should be one of the following magit keywords:
`:actions', `:options', `:keys'.

For example, if you want to use 'g' instead of 'm' for 'grep'
option in a log popup, use:

  (utl-magit-popup-substitute-key 'magit-log-popup :options ?m ?g)"
  (let* ((var-val    (symbol-value var-name))
         (searched-p (lambda (elem) (eq (car elem) old-char)))
         (old-keys   (plist-get var-val property))
         (old-key    (cl-find-if searched-p old-keys)))
    (if old-key
        (let* ((new-key (cl-substitute new-char old-char old-key
                                       :test #'eq))
               (new-keys (cl-substitute-if new-key searched-p old-keys)))
          (set var-name (plist-put var-val property new-keys)))
      (message "Key '%c' does not exist in '%S' %S."
               old-char var-name property))))

;;;###autoload
(defun utl-magit-ido-switch-buffer ()
  "Switch to a magit status buffer using IDO."
  (interactive)
  ;; The code is taken from <https://github.com/magit/magit/issues/1532>.
  (ido-buffer-internal ido-default-buffer-method
                       nil "Magit buffer: " nil "*magit: "))

(provide 'al-magit)

;;; al-magit.el ends here
