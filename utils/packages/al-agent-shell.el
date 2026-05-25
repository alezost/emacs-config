;;; al-agent-shell.el --- Additional functionality `agent-shell'  -*- lexical-binding: t -*-

;; Copyright © 2026 Alex Kost

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

(require 'agent-shell)
(require 'al-buffer)

;;;###autoload
(defun al/agent-shell (&optional arg)
  "Start `agent-shell' if needed or switch to the next `agent-shell' buffer.
If ARG is non-nil, start a new `agent-shell' buffer."
  (interactive "P")
  (if arg
      (agent-shell '(4))
    (al/rotate-or-select-buffer (agent-shell-buffers)
                                #'agent-shell)))

(defun al/agent-next-item-maybe ()
  "Go to the next item if not at the last prompt."
  (interactive)
  (let ((point (point))
        (proc nil))
    (when (or (eq (get-text-property point 'field)
                  'output)
              (null (setq proc (get-buffer-process (current-buffer))))
              (let ((prompt (marker-position (process-mark proc))))
                (< point prompt)))
      (agent-shell-next-item)
      t)))

(al/define-multi-command al/agent-next-item-or-complete
  al/agent-next-item-maybe
  company-complete)

(provide 'al-agent-shell)

;;; al-agent-shell.el ends here
