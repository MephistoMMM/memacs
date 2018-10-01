;;; config.el --- OSX Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq osx-packages
      '(osx-trash))

(when (spacemacs/system-is-mac)
  ;; clean env error while starting
  (if (display-graphic-p)
      (when (string-match-p "/zsh$" (getenv "SHELL"))
        (setq exec-path-from-shell-arguments '("-l"))))
  ;; better default for mac
  (setq mac-pass-command-to-system nil)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  ;; Enable built-in trash support via finder API if available (only on Emacs
  ;; Mac Port)
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t))
  ;; Following statements sometimes doesn't run , so let it be called after user-config
  (spacemacs/defer-until-after-user-config (lambda ()
                                             (add-hook 'focus-in-hook 'memacs/autoescape-use-english-layout)
                                             (add-hook 'focus-out-hook 'memacs/autoescape-recover-outside-layout)
                                             (add-hook 'kill-emacs-hook 'memacs/autoescape-recover-outside-layout)))
  )

(defun osx/init-osx-trash ()
  (use-package osx-trash
    :if (and (spacemacs/system-is-mac)
             (not (boundp 'mac-system-move-file-to-trash-use-finder)))
    :init (osx-trash-setup)))
