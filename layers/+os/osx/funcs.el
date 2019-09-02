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


;;; Counsel Spotlight
;; Incrementally search the Mac spotlight database and open matching
;; files with a swiper search on the query text.
;; Based on http://oremacs.com/2015/07/27/counsel-recoll/

;; Function to be called by counsel-spotlight
;; The onlyin option limits results to my home directory
;; and directories below that
;; mdfind is the command-line interface to spotlight
(defun memacs//counsel-mdfind-function (string &rest _unused)
  "Issue mdfind for STRING."
  (or(counsel-more-chars)
    (let* ((memacs--counsel-search-cmd
            (format "mdfind -onlyin ~/ '%s'" string)))
      (spacemacs//counsel-async-command memacs--counsel-search-cmd)
      nil)))

;; Main function
(defun memacs/counsel-spotlight (&optional initial-input)
  "Search for a string in the mdfind database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read "Spotlight: "
            'memacs//counsel-mdfind-function
            :initial-input initial-input
            :dynamic-collection t
            :history 'counsel-git-grep-history
            :caller 'memacs/counsel-spotlight
            :action (lambda (x)
                      (when (string-match "\\(\/.*\\)\\'" x)
                        (let ((file-name (match-string 1 x)))
                          (find-file file-name)
                          (unless (string-match "pdf$" x)
                            (swiper ivy-text)))))
            :keymap spacemacs--counsel-map
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))))


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
