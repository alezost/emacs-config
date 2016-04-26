;;; al-erc.el --- Additional functionality for ERC

;; Copyright © 2013-2016 Alex Kost

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

(require 'erc)
(require 'erc-log)

;;;###autoload
(defun al/erc-number-of-users ()
  "Show a number of users on the current channel."
  (interactive)
  (let ((channel (erc-default-target)))
    (if (and channel (erc-channel-p channel))
        (message "The number of users on %s: %d"
                 channel
                 (hash-table-count erc-channel-users))
      (user-error "The current buffer is not a channel"))))

(defun al/znc-running-p ()
  "Return non-nil if 'znc' daemon is running."
  (string-match-p "\\`[[:digit:]]+ znc"
                  (shell-command-to-string "pgrep -l znc")))

(defun al/erc-server-buffer-name ()
  "Return a name of buffer with default server."
  (concat (erc-compute-server) ":"
          (number-to-string (erc-compute-port))))

(defun al/erc-server-buffer (&optional noerror)
  "Return the current ERC server buffer.
If NOERROR is non-nil, return nil instead of raising an error if
the server buffer does not exist."
  (or (erc-server-buffer)
      (get-buffer (al/erc-server-buffer-name))
      (unless noerror
        (error "No active ERC server buffer"))))

(defun al/erc-server-buffer-rename ()
  "Rename current server buffer (make a general name)."
  ;; Sometimes we need to modify names like "irc.freenode.net:7000<2>".
  (interactive)
  (let ((old-name (buffer-name))
        (new-name (al/erc-server-buffer-name)))
    (when (string-match (concat (erc-compute-server) ":.*")
                        old-name)
      (rename-buffer new-name)
      (message "Current buffer was renamed from '%s' to '%s'."
               old-name new-name))))

(defun al/erc-switch-to-server-buffer ()
  "Switch to ERC buffer with server."
  (interactive)
  (switch-to-buffer (al/erc-server-buffer)))

;;;###autoload
(defun al/erc-switch-buffer ()
  "Switch to ERC buffer, or start ERC if not already started."
  (interactive)
  (let ((bufs (mapcar #'buffer-name (erc-buffer-list))))
    (if bufs
     	(switch-to-buffer (completing-read "ERC buffer: " bufs))
      (erc))))

;;;###autoload
(defun al/erc-track-switch-buffer (arg)
  "Same as `erc-track-switch-buffer', but start ERC if not already started."
  (interactive "p")
  (let ((buf (al/erc-server-buffer t)))
    (if buf
        (erc-track-switch-buffer arg)
      (erc))))

(defun al/erc-get-channel-buffer-list ()
  "Return a list of the ERC-channel-buffers."
  (erc-buffer-filter
   (lambda () (string-match "^#.*" (buffer-name (current-buffer))))))

;;;###autoload
(defun al/erc-cycle ()
  "Switch to ERC channel buffer, or run `erc-select'.
When called repeatedly, cycle through the buffers."
  (interactive)
  (let ((buffers (al/erc-get-channel-buffer-list)))
    (if buffers
        (progn (when (eq (current-buffer) (car buffers))
                 (bury-buffer)
                 (setq buffers (cdr buffers)))
               (and buffers
                    (switch-to-buffer (car buffers))))
      (call-interactively 'erc-select))))

(defvar al/erc-channel-list '("#emacs" "#erc" "#gnus")
  "A list of channels used in `al/erc-join-channel'.")

(defun al/erc-join-channel (channel &optional key)
  "Join CHANNEL.
Similar to `erc-join-channel', but use `al/erc-channel-list'."
  (interactive
   (list
    (let* ((cur-sexp (thing-at-point 'sexp))
           (chn (if (and cur-sexp
                         (eq 0 (string-match-p "#" cur-sexp)))
                    cur-sexp
                  "#")))
      (completing-read "Join channel: " al/erc-channel-list nil nil chn))
    (when (or current-prefix-arg erc-prompt-for-channel-key)
      (read-from-minibuffer "Channel key (RET for none): " nil))))
  (with-current-buffer (al/erc-server-buffer)
    (erc-cmd-JOIN channel (when (>= (length key) 1) key))))

(defun al/erc-quit-server (reason)
  "Disconnect from current server.
Similar to `erc-quit-server', but without prompting for REASON."
  (interactive (list ""))
  (with-current-buffer (al/erc-server-buffer)
    (erc-cmd-QUIT reason)))

(defun al/erc-ghost-maybe (server nick)
  "Send GHOST message to NickServ if NICK ends with `erc-nick-uniquifier'.
The function is suitable for `erc-after-connect'."
  (when (string-match (format "\\(.*?\\)%s+$" erc-nick-uniquifier) nick)
    (let ((nick-orig (match-string 1 nick))
          (password erc-session-password))
      (erc-message "PRIVMSG" (format "NickServ GHOST %s %s"
                                     nick-orig password))
      (erc-cmd-NICK nick-orig)
      (erc-message "PRIVMSG" (format "NickServ IDENTIFY %s %s"
                                     nick-orig password)))))

(defun al/erc-insert-timestamp (string)
  "Insert timestamps in the beginning of the line.

This function is suitable for `erc-insert-timestamp-function'.
It is a sort of combination of `erc-insert-timestamp-left' and
`erc-insert-timestamp-left-and-right'.  Usual
timestamps (`erc-timestamp-format') are inserted in the beginning
of each line and an additional
timestamp (`erc-timestamp-format-left') is inserted only if it
was changed since the last time (by default if the date was
changed)."
  (goto-char (point-min))
  (erc-put-text-property 0 (length string) 'field 'erc-timestamp string)
  (insert string)
  (let ((stamp (erc-format-timestamp (current-time)
                                     erc-timestamp-format-left)))
    (unless (string-equal stamp erc-timestamp-last-inserted-left)
      (goto-char (point-min))
      (erc-put-text-property 0 (length stamp) 'field 'erc-timestamp stamp)
      (insert stamp)
      (setq erc-timestamp-last-inserted-left stamp))))


;;; Away

(defvar al/erc-away-msg-list '("just away" "learning emacs" "sleeping")
  "A list of away messages for `al/erc-away'.")

(defun al/erc-away (&optional reason)
  "Mark the user as being away.
Interactively prompt for reason; with prefix mark as unaway.
Reasons are taken from `al/erc-away-msg-list'."
  (interactive
   (list (if current-prefix-arg
             ""
           (completing-read "Reason for AWAY: "
                            al/erc-away-msg-list))))
  (with-current-buffer (al/erc-server-buffer)
    (erc-cmd-AWAY (or reason ""))))

(defun al/erc-away-time ()
  "Return non-nil if the current ERC process is set away.
Similar to `erc-away-time', but no need to be in ERC buffer."
  (with-current-buffer (al/erc-server-buffer)
    (erc-away-time)))


;;; CTCP info

(defun al/erc-ctcp-query-FINGER (proc nick login host to msg)
  "Respond to a CTCP FINGER query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice nick "FINGER Keep your FINGER out of me."))
  nil)

(defun al/erc-ctcp-query-ECHO (proc nick login host to msg)
  "Respond to a CTCP ECHO query."
  (when (string-match "^ECHO\\s-+\\(.*\\)\\s-*$" msg)
    (let ((str (apply #'string
                      (reverse (string-to-list (match-string 1 msg))))))
      (unless erc-disable-ctcp-replies
	(erc-send-ctcp-notice nick (format "ECHO Did you mean '%s'?" str)))))
  nil)

(defun al/erc-ctcp-query-TIME (proc nick login host to msg)
  "Respond to a CTCP TIME query."
  (unless erc-disable-ctcp-replies
    (let* ((hour (nth 2 (decode-time (current-time))))
           (str (cond ((al/erc-away-time) "time to be away")
                      ((>= hour 18) "almost night")
                      ((>= hour 12) (format-time-string "%A"))
                      ((>= hour 6)  "always morning")
                      (t            "time to sleep"))))
      (erc-send-ctcp-notice nick (format "TIME %s." str))))
  nil)

(defun al/erc-ctcp-query-VERSION (proc nick login host to msg)
  "Respond to a CTCP VERSION query."
  (unless erc-disable-ctcp-replies
    (erc-send-ctcp-notice
     nick
     (format "VERSION ERC (GNU Emacs %s)" emacs-version)))
  nil)


;;; Log

(defun al/erc-view-log-file ()
  "Visit a log file for the current ERC buffer."
  (interactive)
  (view-file (erc-current-logfile)))

(defun al/erc-log-file-name-network-channel (buffer target nick server port)
  "Return erc log-file name of network (or server) and channel names.
The result file name is in the form \"network_channel.txt\".
This function is suitable for `erc-generate-log-file-name-function'."
  (with-current-buffer buffer
    (let* ((target (erc-default-target)) ; nil for server buffer
           (file (concat (or (erc-network-name) server)
                         (and target (concat "_" target))
                         ".txt")))
      ;; We need a make-safe-file-name function.
      (convert-standard-filename file))))

;; If you want to exclude a particular channel "#foochannel" and
;; channels that have "beard" in their names, use the following:
;;
;; (setq al/erc-log-excluded-regexps '("\\`#foochannel" "beard"))
;; (setq erc-enable-logging 'al/erc-log-all-but-some-buffers)
;;
;; Note: channel buffers may have names like "#foobar<2>", so too strict
;; regexps like "\\`#foochannel\\'" may be not good.

(defvar al/erc-log-excluded-regexps nil
  "List of regexps for erc buffer names that will not be logged.")

(defun al/erc-log-all-but-some-buffers (buffer)
  "Return t if logging should be enabled for BUFFER.
Use `al/erc-log-excluded-regexps' to check if BUFFER should be
logged or not.
The function is intended to be used for `erc-enable-logging'."
  (cl-notany (lambda (re)
               (string-match-p re (buffer-name buffer)))
             al/erc-log-excluded-regexps))

(provide 'al-erc)

;;; al-erc.el ends here
