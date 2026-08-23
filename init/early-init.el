;;; early-init.el --- File symlinked by `early-init-file'  -*- lexical-binding: t -*-

(defvar al/before-init-time (current-time)
  "Current time at the very start of my config loading.
I.e., at the start of `early-init-file'.")

(defvar al/after-init-time nil
  "Current time at the very end of my config loading.
I.e., at the end of `after-init-hook'.")

;; Hacks to reduce the startup time:
;; <https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/>
;; <https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/>
(setq
 gc-cons-threshold (expt 2 24)  ; 16 MiB
 garbage-collection-messages t
 package-enable-at-startup nil
 ;; Show all loading messages (by default, `require' suppresses messages).
 ;; Actually, this setting increases startup time because each additional
 ;; message forces echo area redrawing.
 force-load-messages t)

;; Set `file-name-handler-alist' to nil until Emacs startup is finished.
(defvar al/file-name-handler-alist file-name-handler-alist
  "Original value of `file-name-handler-alist'.")
(setq file-name-handler-alist nil)

(defun al/restore-file-name-handler-alist ()
  "Restore original value of `file-name-handler-alist'."
  (setq file-name-handler-alist
        (append file-name-handler-alist
                al/file-name-handler-alist)))

(add-hook 'emacs-startup-hook #'al/restore-file-name-handler-alist)

;;; early-init.el ends here
