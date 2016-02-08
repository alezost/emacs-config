;;; al-pcmpl-args.el --- Additional functionality for pcmpl-args

;; Author: Alex Kost <alezost@gmail.com>
;; Created: 23 Jul 2015

;;; Code:

(require 'pcmpl-args)

;;;###autoload
(defun pcomplete/pre-inst-env ()
  (pcmpl-args-pcomplete
   (pcmpl-args-make-argspecs
    `((argument 0 (("COMMAND" nil))
                :subparser pcmpl-args-command-subparser)))))

(provide 'al-pcmpl-args)

;;; al-pcmpl-args.el ends here
