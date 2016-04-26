;;; al-ediff.el --- Additional functionality for ediff

;; Copyright © 2014-2016 Alex Kost

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

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
