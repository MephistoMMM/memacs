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


;;; Spotlight

(defun memacs/spotlight-fast-in-home ()
  (interactive)
  (spotlight-fast nil))


;;; Switch To Item2

(defun memacs/send-region-to-iterm2 (start end)
  "Send the text of the current region to iTerm2."
  (interactive "r")
  (let ((python (if (file-exists-p memacs-iterm2-pyenv-python-interpreter)
                    memacs-iterm2-pyenv-python-interpreter
                  memacs-iterm2-python-fallback-interpreter))
        (script (concat memacs-iterm2-scripts-path
                        "run_command_in_iterm2.py")))
    (call-process-region start end python nil nil nil script)))

(defun memacs//run-command-in-iterm2 (CMD &rest args)
  "Send the commands to iTerm2."
  (let ((python (if (file-exists-p memacs-iterm2-pyenv-python-interpreter)
                    memacs-iterm2-pyenv-python-interpreter
                  memacs-iterm2-python-fallback-interpreter))
        (script (concat memacs-iterm2-scripts-path
                        "run_command_in_iterm2.py")))
    (apply 'call-process python nil "*Iterm2Log*" nil script CMD args)))

(defun memacs//go-to-dir-in-iterm2 (path)
  "Go to directory in iTerm2."
  (let ((python (if (file-exists-p memacs-iterm2-pyenv-python-interpreter)
                    memacs-iterm2-pyenv-python-interpreter
                  memacs-iterm2-python-fallback-interpreter))
        (script (concat memacs-iterm2-scripts-path
                        "go_to_session.py")))
    (call-process python nil "*Iterm2Log*" nil script path)))

(defun memacs/switch-to-item2 ()
  "Open item2."
  (interactive)
  (let ((python (if (file-exists-p memacs-iterm2-pyenv-python-interpreter)
                    memacs-iterm2-pyenv-python-interpreter
                  memacs-iterm2-python-fallback-interpreter))
        (script (concat memacs-iterm2-scripts-path
                        "go_to_session.py")))
    (call-process python nil "*Iterm2Log*" nil script))
  )

(defun memacs/switch-to-item2-on-dir-of-current-buffer()
  "Open item2 then cd into the path of the directory of
current buffer."
  (interactive)
  (memacs//go-to-dir-in-iterm2 (shell-quote-argument
                                (or default-directory "~")))
  )

(defun memacs/switch-to-item2-on-dir-of-current-project()
  "Open item2 then cd into the path of the root directory of
current project."
  (interactive)
  (if  (boundp 'projectile-project-root)
      (let ((current-project-directory (projectile-project-root)))
        (memacs//go-to-dir-in-iterm2 (shell-quote-argument
                                      (or current-project-directory "~"))))
    (message "FUNC projectile-project-root doesn't exist, do nothing!"))
  )



;;; Autoescape
(defun memacs/autoescape-use-english-layout()
  "Change input source to english layout while emacs frame focused."
  (unless (evil-hybrid-state-p)
    (let ((origin-layout (shell-command-to-string "textinputsource")))
      (unless (string= origin-layout
                       memacs-autoescape-english-layout-name)
        (start-process-shell-command
         "changeInputSource" nil
         (concat "textinputsource -s "
                 memacs-autoescape-english-layout-name))
        )))
  )
