;;; config.el --- OSX Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun memacs/autoescape-use-english-layout()
  "Change input source to english layout while emacs frame focused."
  (unless (evil-hybrid-state-p)
    (setq memacs-autoescape--origin-outside-layout-name (shell-command-to-string "textinputsource"))
    (unless (string= memacs-autoescape--origin-outside-layout-name
                     memacs-autoescape-english-layout-name)
      (start-process-shell-command "changeInputSource" nil
                                   (concat "textinputsource -s "
                                           memacs-autoescape-english-layout-name))
      ))
  )

(defun memacs/autoescape-recover-outside-layout()
  "Recover input source to origin layout while emacs frame unfocused."
  (unless (string=
           (shell-command-to-string "textinputsource")
           memacs-autoescape--origin-outside-layout-name)
    (call-process-shell-command (concat "textinputsource -s "
                                         memacs-autoescape--origin-outside-layout-name)))
  )
