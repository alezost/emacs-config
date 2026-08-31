;;; save-place.el --- Automatically save point positions in files  -*- lexical-binding:t -*-

;; Copyright (C) 1993-1994, 2001-2026 Free Software Foundation, Inc.
;; Copyright (C) 2026 Alex Kost

;; Author: Karl Fogel <kfogel@red-bean.com>
;; Created: July, 1993
;; Keywords: bookmarks, placeholders

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a simplified version of the original `saveplace' package
;; taken from Emacs 31.  It just provides the raw "save position"
;; functionality for files.  The code is largely simplified and
;; polished, all extra features removed: no `dired' support, no support
;; for abbreviated file names, no autosave support, etc.

;; This file provides `save-place-mode', a minor mode to save point
;; positions in files, so that visiting them later (even during a
;; different Emacs session) automatically moves point to the saved
;; position.

;;; Code:

(defgroup save-place nil
  "Automatically save point positions in files."
  :group 'data)

(defcustom save-place-file
  (expand-file-name "places" user-emacs-directory)
  "Name of the file with saved places."
  :type 'file)

(defcustom save-place-limit 400
  "Maximum number of places to save.
If nil, there is no limit."
  :type '(choice (integer :tag "Entries" :value 1)
		 (const :tag "No Limit" nil)))

(defcustom save-place-ignore-files-regexp
  "COMMIT_EDITMSG"
  "Regexp matching files whose positions should not be saved.
If nil, no files are excluded."
  :type '(choice (const :tag "Don't exclude any files" nil)
                 regexp))

(defvar save-place-alist nil
  "Alist of saved places.
Each element has (FILENAME . POSITION) form.")

(defvar save-place-loaded nil
  "Non-nil means that the `save-place-file' has been loaded.")

(defun save-place-load-from-file ()
  "Load file positions from `save-place-file'."
  (interactive)
  (let ((file (expand-file-name save-place-file)))
    (when (file-readable-p file)
      (with-temp-buffer
        ;; Make sure our 'coding:' cookie in the save-place
        ;; file will take effect, in case the caller binds
        ;; coding-system-for-read.
        (let (coding-system-for-read)
          (insert-file-contents file))
        (goto-char (point-min))
        (setq save-place-alist
              (with-demoted-errors "Error reading `save-place-file': %S"
                (car (read-from-string
                      (buffer-substring (point-min) (point-max))))))
        (when (and save-place-limit
                   (< 0 save-place-limit (length save-place-alist)))
          (setq save-place-alist
                (seq-subseq save-place-alist
                            0 save-place-limit)))))))

(defun save-place-save-to-file ()
  "Save file positions to `save-place-file'."
  (let ((file (expand-file-name save-place-file))
        (coding-system-for-write 'utf-8))
    (with-temp-buffer
      (insert (format ";;; -*- coding: %s; mode: lisp-data -*-\n"
                      coding-system-for-write))
      (let ((print-length nil)
            (print-level nil))
        (prin1 save-place-alist (current-buffer)))
      (condition-case nil
          ;; Don't use write-file; we don't want this buffer to visit it.
          (write-region (point-min) (point-max) file nil
                        (unless (called-interactively-p 'interactive) 'quiet))
        (file-error (message "Saving places: can't write %s" file))))))

(defun save-place-initialize-maybe ()
  "Call `save-place-load-from-file' if needed."
  (unless save-place-loaded
    (setq save-place-loaded t)
    (save-place-load-from-file)))

(defun save-place-save-position ()
  "Add the current file position to `save-place-alist'."
  (save-place-initialize-maybe)
  (when (and buffer-file-name
             (or (not save-place-ignore-files-regexp)
                 (not (string-match-p save-place-ignore-files-regexp
                                      buffer-file-name))))
    (let ((cell (assoc buffer-file-name save-place-alist))
          (pos  (if (eq major-mode 'hexl-mode)
                    (with-no-warnings
                      (1+ (hexl-current-address)))
                  (point))))
      (if cell
          (setcdr cell pos)
        (unless (= pos 1)       ; optimize out the degenerate case
          (push (cons buffer-file-name pos)
                save-place-alist))))))

(defun save-place-restore-position ()
  "Restore the current file position from `save-place-alist'."
  (save-place-initialize-maybe)
  (or revert-buffer-in-progress-p
      (when-let* ((cell (and (stringp buffer-file-name)
                             (assoc buffer-file-name
                                    save-place-alist))))
        (goto-char (cdr cell)))))

(defun save-place-save-everything ()
  "Save all visited file positions to `save-place-file'."
  (interactive)
  (when save-place-loaded
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (and buffer-file-name
                   (not find-file-literally))
          (save-place-save-position))))
    (save-place-save-to-file)))

;;;###autoload
(define-minor-mode save-place-mode
  "Automatically save point positions in visited files.
This means when you visit a file, point goes to the last place
where it was when you previously visited the same file."
  :global t
  (let ((hook-fun (if save-place-mode #'add-hook #'remove-hook)))
    (funcall hook-fun 'find-file-hook   #'save-place-restore-position)
    (funcall hook-fun 'kill-buffer-hook #'save-place-save-position)
    (funcall hook-fun 'kill-emacs-hook  #'save-place-save-everything)))

(provide 'save-place)

;;; save-place.el ends here
