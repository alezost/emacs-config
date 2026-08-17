;;; appt.el --- Settings for `appt' package  -*- lexical-binding: t -*-

(eval-when-compile
  (require 'al-aux-macros))

(require 'appt)
(require 'al-places)
(require 'al-appt)

(setq
 appt-audible nil
 appt-display-diary nil
 appt-message-warning-time 5
 appt-display-interval 1)

(al/setq-file
 al/appt-notify-normal-sound (al/sound-dir-file "drums.wav")
 al/appt-notify-urgent-sound (al/sound-dir-file "bell.oga"))

(advice-add 'appt-display-message :override #'al/appt-display-message)
(advice-add 'appt-mode-line :override #'al/appt-mode-line)

;;; appt.el ends here
