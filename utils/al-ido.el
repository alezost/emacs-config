;;; al-ido.el --- Additional functionality for ido-mode

;; Copyright © 2013–2017, 2020 Alex Kost

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

(require 'ido)

(defvar ido-rotate-temp)

;;;###autoload
(defun al/ido-set-current-directory (dir)
  "Change the current ido working directory to DIR."
  (interactive)
  (ido-set-current-directory dir)
  (setq ido-exit 'refresh)
  (setq ido-text-init ido-text)
  (setq ido-rotate-temp t)
  (exit-minibuffer))

;;;###autoload
(defun al/ido-copy-current-item ()
  "Put the current ido item into `kill-ring'."
  (interactive)
  (kill-new (car ido-matches)))

(defun al/ido-completions (name)
  "Replacement for `ido-completions' (with fixed colors)."
  (with-no-warnings
    (defvar ido-show-confirm-message)
    (defvar ido-directory-nonreadable)
    (defvar ido-directory-too-big))
  (let* ((comps ido-matches)
	 (ind (and (consp (car comps)) (> (length (cdr (car comps))) 1)
		   ido-merged-indicator))
	 first)

    (if (and ind ido-use-faces)
	(put-text-property 0 1 'face 'ido-indicator ind))

    (if (and ido-use-faces comps)
	(let* ((fn (ido-name (car comps)))
	       (ln (length fn)))
	  (setq first (copy-sequence fn))
          ;; XXX ↓ The only changed sexp.  Add the face, not put as I
          ;; don't want the existing face to be overwritten.
          (add-face-text-property 0 ln 'ido-first-match nil first)
	  ;; (put-text-property 0 ln 'face
	  ;;       	     (if (= (length comps) 1)
          ;;                        (if ido-incomplete-regexp
          ;;                            'ido-incomplete-regexp
          ;;                          'ido-only-match)
	  ;;       	       'ido-first-match)
	  ;;       	     first)
	  (if ind (setq first (concat first ind)))
	  (setq comps (cons first (cdr comps)))))

    (cond ((null comps)
	   (cond
	    (ido-show-confirm-message
	     (or (nth 10 ido-decorations) " [Confirm]"))
	    (ido-directory-nonreadable
	     (or (nth 8 ido-decorations) " [Not readable]"))
	    (ido-directory-too-big
	     (or (nth 9 ido-decorations) " [Too big]"))
	    (ido-report-no-match
	     (nth 6 ido-decorations))  ;; [No match]
	    (t "")))
	  (ido-incomplete-regexp
           (concat " " (car comps)))
	  ((null (cdr comps))		;one match
	   (concat (if (if (not ido-enable-regexp)
                           (= (length (ido-name (car comps))) (length name))
                         ;; We can't rely on the length of the input
                         ;; for regexps, so explicitly check for a
                         ;; complete match
                         (string-match name (ido-name (car comps)))
                         (string-equal (match-string 0 (ido-name (car comps)))
                                       (ido-name (car comps))))
                       ""
                     ;; When there is only one match, show the matching file
                     ;; name in full, wrapped in [ ... ].
                     (concat
                      (or (nth 11 ido-decorations) (nth 4 ido-decorations))
                      (ido-name (car comps))
                      (or (nth 12 ido-decorations) (nth 5 ido-decorations))))
		   (if (not ido-use-faces) (nth 7 ido-decorations))))  ;; [Matched]
	  (t				;multiple matches
	   (let* ((items (if (> ido-max-prospects 0) (1+ ido-max-prospects) 999))
		  (alternatives
		   (apply
		    #'concat
		    (cdr (apply
			  #'nconc
			  (mapcar
			   (lambda (com)
			     (setq com (ido-name com))
			     (setq items (1- items))
			     (cond
			      ((< items 0) ())
			      ((= items 0) (list (nth 3 ido-decorations))) ; " | ..."
			      (t
			       (list (nth 2 ido-decorations) ; " | "
				     (let ((str (substring com 0)))
				       (if (and ido-use-faces
						(not (string= str first))
						(ido-final-slash str))
					   (put-text-property 0 (length str) 'face 'ido-subdir str))
				       str)))))
			   comps))))))

	     (concat
	      ;; put in common completion item -- what you get by pressing tab
	      (if (and (stringp ido-common-match-string)
		       (> (length ido-common-match-string) (length name)))
		  (concat (nth 4 ido-decorations)   ;; [ ... ]
			  (substring ido-common-match-string (length name))
			  (nth 5 ido-decorations)))
	      ;; list all alternatives
	      (nth 0 ido-decorations)  ;; { ... }
	      alternatives
	      (nth 1 ido-decorations)))))))

(provide 'al-ido)

;;; al-ido.el ends here
