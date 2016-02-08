;;; al-ediff.el --- Additional functionality for ediff

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 1 Nov 2014

;;; Code:

;; Idea of saving and restoring window configuration came from
;; <http://www.emacswiki.org/emacs/EdiffMode>.  To use this feature I
;; added the following code to my emacs config:
;;
;;   (add-hook 'ediff-before-setup-hook
;;             'al/ediff-save-window-configuration)
;;   (add-hook 'ediff-quit-hook
;;             'al/ediff-restore-window-configuration
;;             'append)

(defvar al/ediff-window-configuration nil
  "Window configuration to be restored after ediff session.")

(defun al/ediff-save-window-configuration ()
  "Save window configuration before ediff session."
  (setq al/ediff-window-configuration (current-window-configuration)))

(defun al/ediff-restore-window-configuration ()
  "Restore saved window configuration after ediff session."
  (set-window-configuration al/ediff-window-configuration))

(provide 'al-ediff)

;;; al-ediff.el ends here
