;;; al-erc-cmd.el --- Additional commands for `erc' package  -*- lexical-binding: t -*-

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

;;; Commentary:

;; This file contains "entry point" commands for `erc' package to avoid
;; recursive loading.  See `al-eshell-cmd' commentary for details.

;;; Code:

(require 'erc)
(require 'al-buffer)
(require 'al-erc)

;;;###autoload
(defun al/erc-switch-buffer ()
  "Switch to ERC buffer, or start ERC if not already started."
  (interactive)
  (al/rotate-or-select-buffer #'al/erc-buffers #'erc "ERC buffer: ")
  (recenter-top-bottom 2))

;;;###autoload
(defun al/erc-track-switch-buffer (arg)
  "Same as `erc-track-switch-buffer', but start ERC if not already started."
  (interactive "p")
  (if (al/erc-server-buffer t)
      (progn
        (erc-track-switch-buffer arg)
        (recenter-top-bottom 2))
    (erc)))

(provide 'al-erc-cmd)

;;; al-erc-cmd.el ends here
