;;; early-init.el --- File symlinked by `early-init-file'  -*- lexical-binding: t -*-

;; Hacks to reduce the startup time:
;; <https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/>
;; <https://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/>
(setq
 gc-cons-threshold (expt 2 24)  ; 16 MiB
 garbage-collection-messages t
 package-enable-at-startup nil)

;; Set `file-name-handler-alist' to nil until Emacs startup is finished.
(defvar al/file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defun al/restore-file-name-handler-alist ()
  (setq file-name-handler-alist
        (append file-name-handler-alist
                al/file-name-handler-alist) ))

(add-hook 'emacs-startup-hook #'al/restore-file-name-handler-alist)

;;; early-init.el ends here
