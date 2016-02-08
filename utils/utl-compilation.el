;;; utl-compilation.el --- Additional functionality for compilation buffers

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 19 Mar 2015

;;; Code:

(require 'cl-lib)
(require 'notifications)
(require 'utl-notification)

(defvar utl-compilation-sound-success nil
  "Sound file for a successful compilation.")

(defvar utl-compilation-sound-error nil
  "Sound file for a failed compilation.")

(defvar exit-status)

;; Idea from <https://gist.github.com/jwiegley/fd78e747f27a90cb67a2>.
(defun utl-compilation-notify (buffer reason)
  "Notify about the ended compilation in BUFFER.
This function is intended to be used in
`compilation-finish-functions'."
  (with-current-buffer buffer
    (unless (eq major-mode 'grep-mode)
      (cl-multiple-value-bind (sound urgency)
          (if (= exit-status 0)
              (list utl-compilation-sound-success 'normal)
            (list utl-compilation-sound-error 'critical))
        (and sound (utl-play-sound sound))
        (notifications-notify
         :urgency urgency
         :title "Compilation finished"
         :body (format (concat "Buffer: %s\n"
                               "Reason: %s")
                       (buffer-name buffer) reason))))))

(provide 'utl-compilation)

;;; utl-compilation.el ends here
