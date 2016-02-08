;;; utl-ediff.el --- Additional functionality for ediff

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 1 Nov 2014

;;; Code:

;; Idea of saving and restoring window configuration came from
;; <http://www.emacswiki.org/emacs/EdiffMode>.  To use this feature I
;; added the following code to my emacs config:
;;
;;   (add-hook 'ediff-before-setup-hook
;;             'utl-ediff-save-window-configuration)
;;   (add-hook 'ediff-quit-hook
;;             'utl-ediff-restore-window-configuration
;;             'append)

(defvar utl-ediff-window-configuration nil
  "Window configuration to be restored after ediff session.")

(defun utl-ediff-save-window-configuration ()
  "Save window configuration before ediff session."
  (setq utl-ediff-window-configuration (current-window-configuration)))

(defun utl-ediff-restore-window-configuration ()
  "Restore saved window configuration after ediff session."
  (set-window-configuration utl-ediff-window-configuration))

(provide 'utl-ediff)

;;; utl-ediff.el ends here
