;;; al-eshell.el --- Additional functionality for eshell  -*- lexical-binding: t -*-

;; Copyright © 2013–2026 Alex Kost

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

;;; Code:

(eval-when-compile
  (require 'fp-utils)
  (require 'let-macros))
(require 'em-alias)
(require 'em-dirs)
(require 'em-unix)
(require 'em-prompt)
(require 'al-general)
(require 'al-places)
(require 'al-buffer)
(require 'al-shell)     ; for `al/shell-set-comment-variables'
(require 'al-url)
(require 'al-visual)

(defun al/eshell-buffers (&optional no-sort)
  "Return a list of all eshell buffers.
If NO-SORT is non-nil, do not sort the list by buffer names."
  (al/buffers-by-mode 'eshell-mode
                      (unless no-sort #'al/buffer-name<)))

;;;###autoload
(defun al/eshell (&optional arg)
  "Start eshell if needed or switch to the next \\[eshell] buffer.
If ARG is non-nil, start a new eshell buffer."
  (interactive "P")
  (if arg
      (eshell 'new)
    (al/rotate-or-select-buffer (al/eshell-buffers) #'eshell)))

;;;###autoload
(defun al/eshell-cd (arg)
  "Start eshell and change directory there to the current one.
ARG has the same meaning as in `eshell'"
  (interactive "P")
  (let ((dir default-directory))
    (eshell arg)
    (eshell/cd dir)))

(defun al/eshell-refresh-aliases ()
  "Refresh aliases for the current eshell buffer."
  (interactive)
  (eshell-alias-initialize))

(defun al/eshell-kill-whole-line (arg)
  "Similar to `kill-whole-line', but respect eshell prompt."
  (interactive "p")
  (if (< (point) eshell-last-output-end)
      (kill-whole-line arg)
    (kill-region eshell-last-output-end
                 (progn (forward-line arg) (point)))))

(declare-function Info-find-node "info" t)
(declare-function Info-menu "info" t)

(defun al/eshell/info (&rest args)
  "Run `info' command on NAME.
NAME is the car of ARGS.

This function is intended to be used as a substitution for
`eshell/info'.  It does the following:

  info       =>  go to top info window;
  info NAME  =>  if NAME is a file '*.info', visit it;
  info NAME  =>  otherwise go to top info node and then menu item NAME."
  (require 'info)
  (if-let ((name (car args)
                 (<= #'stringp
                     (cut #'string-match "\\.info" <>)))
           (file (expand-file-name name)
                 (<= #'file-exists-p)))
      (Info-find-node file "Top")
    (Info-directory)
    (Info-menu name)))


;;; Replacing eshell commands

(defun al/eshell-command (command &rest args)
  "Return parsed eshell object for COMMAND and its ARGS."
  ;; Taken from `eshell-parse-command' output, e.g.:
  ;; (eshell-parse-command "echo" '("hello"))
  `(eshell-with-copied-handles
    (eshell-trap-errors
     (eshell-named-command ,command (list ,@args)))))

(defun al/eshell-title-command (format-string &rest args)
  "Return parsed eshell object with title string."
  (al/eshell-command "echo" (apply #'al/title-string format-string args)))

(defun al/eshell-replace-command (&rest body)
  "Replace the current eshell command with BODY.
BODY should consist of parsed eshell commands."
  ;; Originates from `eshell-maybe-replace-by-alias'.
  (throw 'eshell-replace-command
         `(let ((eshell-command-name      ',eshell-last-command-name)
                (eshell-command-arguments ',eshell-last-arguments))
            (progn ,@body))))


;;; ytdlp command

(defvar-local al/eshell-ytdlp-file-name nil
  "File name of the currently downloading file.")

(defun al/eshell-ytdlp (&rest args)
  "Download file using `yt-dlp' shell command with ARGS.
If the last argument is not URL, use it as an EMMS playlist name and add
the downloaded file to this playlist.  See `al/emms-get-playlist' to
find out how a playlist name may look like."
  (if-letn ((args (nreverse args))
            (last-arg (car args))
            (last-arg-is-url? (al/check-url last-arg))
            (args (nreverse (if last-arg-is-url? args (cdr args))))
            (playlist (unless last-arg-is-url? last-arg)))
      (let ((get-file-args `("--no-warnings" "--print" "filename" ,@args))
            (download-args `("-o" al/eshell-ytdlp-file-name ,@args)))
        (require 'al-text)
        (require 'al-emms)
        ;; XXX Do not try to improve the following code.  In particular, do
        ;; not replace `al/eshell-ytdlp-file-name' with local variable.  How
        ;; eshell internals work is a mystery: `let*' (and more complex
        ;; structures) does not work at all inside
        ;; `al/eshell-replace-command', `let' works but unreliable.
        (al/eshell-replace-command
         (al/eshell-title-command "Requesting file name...")
         `(setq al/eshell-ytdlp-file-name
                (al/download-dir-file
                 (al/parse-ytdlp-file-name-output
                  (eshell-command-to-value
                   (eshell-as-subcommand
                    ,(apply #'al/eshell-command "yt-dlp" get-file-args))))))
         (al/eshell-command "echo" 'al/eshell-ytdlp-file-name)
         (al/eshell-title-command "Downloading the file...")
         (apply #'al/eshell-command "yt-dlp" download-args)
         (al/eshell-title-command "Adding the file to playlist...")
         (al/eshell-command "al/emms-add-file-to-playlist"
                            playlist 'al/eshell-ytdlp-file-name)))
    (al/eshell-replace-command
     (al/eshell-title-command "Running yt-dlp command...")
     (apply #'al/eshell-command "yt-dlp" `("--paths" ,al/download-dir ,@args)))))


;;; Miscellaneous commands

(defun al/eshell-run-command-from-env (name &optional pre post)
  "Run shell command from environment variable NAME.
PRE and POST are lists of additional strings prepended and appended to
the shell command."
  (if-let ((cmd (getenv name)))
      (al/eshell-replace-command
       (apply #'al/eshell-command
              (append pre (split-string cmd " ") post)))
    (error "Environment variable `%s' does not exist" name)))

(defun al/eshell-mpv (&rest args)
  "Run `MPV_CMD' environment variable with ARGS."
  (al/eshell-run-command-from-env "MPV_CMD" nil args))

(defun al/eshell-tor-mpv (&rest args)
  "Run `MPV_CMD' environment variable with ARGS using `torsocks'."
  (al/eshell-run-command-from-env "MPV_CMD" '("torsocks") args))


;;; Prompt

;; Idea from <http://www.emacswiki.org/emacs/EshellPrompt>.

(defun al/eshell-prompt ()
  "Function for `eshell-prompt-function'."
  (format "%s %s%s%s %s\n%s "
          (al/with-face 'font-lock-comment-face
            (format-time-string "%H:%M" (current-time)))
          (eshell/whoami)
          (al/with-face 'escape-glyph "@")
          (system-name)
          ;; Using `font-lock-function-name-face' instead
          ;; `dired-directory' because `dired' may not be loaded yet.
          (al/with-face 'font-lock-function-name-face
            (abbreviate-file-name (eshell/pwd)))
          (al/with-face 'comint-highlight-prompt
            (if (= (user-uid) 0) "#" "$"))))


;;; Input (command) line

(defun al/eshell-input-at-point ()
  "Return eshell input from the current input (command) line.
Return nil, if the current line is not the input line."
  (let ((bol (pos-bol))
        (eol (pos-eol)))
    (and (eq 'prompt (get-text-property bol 'field))
         (null (get-text-property eol 'field))
         (buffer-substring-no-properties
          (save-excursion (goto-char eol) (line-beginning-position))
          eol))))

;;;###autoload
(defun al/eshell-send-input-maybe ()
  "Call `eshell-send-input' if the point is on the command line."
  (interactive)
  (when (< (point) eshell-last-output-end)
    (if-let ((input (al/eshell-input-at-point)))
        (progn
          (goto-char eshell-last-output-end)
          (delete-region eshell-last-output-end (point-max))
          (insert input))
      (user-error (substitute-command-keys "\
You don't want to do \"\\[al/eshell-send-input-maybe]\" here"))))
  (eshell-send-input))


;;; History

(require 'em-hist)

;;;###autoload
(defun al/eshell-previous-matching-input-from-input (arg)
  "Search backwards through input history for match for current input.
Unlike `eshell-previous-matching-input-from-input', the matching
input is not forced to begin with the current input."
  (interactive "p")
  (unless (memq last-command '(al/eshell-previous-matching-input-from-input
                               al/eshell-next-matching-input-from-input))
    ;; Starting a new search.
    (setq eshell-matching-input-from-input-string
          (buffer-substring (save-excursion (beginning-of-line) (point))
                            (point))
          eshell-history-index nil))
  (eshell-previous-matching-input
   (regexp-quote eshell-matching-input-from-input-string)
   arg))

;;;###autoload
(defun al/eshell-next-matching-input-from-input (arg)
  "Search forwards through input history for match for current input."
  (interactive "p")
  (al/eshell-previous-matching-input-from-input (- arg)))

;;;###autoload
(defun al/eshell-save-history ()
  "Save history of the current Eshell buffers."
  (interactive)
  (eshell-save-some-history))


;;; Miscellaneous

(defun al/eshell-set-paragraph ()
  ;; Default value of `paragraph-separate' breaks
  ;; `eshell-next-prompt'/`eshell-previous-prompt'.
  (setq-local paragraph-separate "useLESS var"))

(declare-function shell--parse-pcomplete-arguments "shell")

(defun al/eshell-set-parse-function ()
  (when (and (require 'shell nil t)
             (fboundp 'shell--parse-pcomplete-arguments))
    ;; Default file completions in `eshell' are horrible.  The default
    ;; parsing function, `eshell-complete-parse-arguments', ignores
    ;; everything that is placed after point.  This, for example, leads to
    ;; the following completion: "cd uti|ls" → "cd utils/|ls", while with
    ;; shell parsing, it will be "cd utils/|" as expected.
    (setq-local pcomplete-parse-arguments-function
                #'shell--parse-pcomplete-arguments)))

(defun al/eshell-set-local-variables ()
  "Set missing local variables for `eshell-mode'."
  (al/eshell-set-paragraph)
  (al/shell-set-comment-variables)
  (al/eshell-set-parse-function))

(provide 'al-eshell)

;;; al-eshell.el ends here
