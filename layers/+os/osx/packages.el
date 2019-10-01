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
  ;; Keybinding prevent passing command to system
  (setq mac-pass-command-to-system nil)

  ;; Title
  (setq frame-title-format '("Memacs Emacs - %b")
        icon-title-format frame-title-format)

  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . dark))
  (add-hook 'after-load-theme-hook
            (lambda ()
              (let ((bg (frame-parameter nil 'background-mode)))
                (set-frame-parameter nil 'ns-appearance bg)
                (setcdr (assq 'ns-appearance default-frame-alist) bg))))

  ;; Menu/Tool/Scroll bars
  (unless (version<= "27" emacs-version)
    ;; Move to early init-file in 27
    (unless (spacemacs/system-is-mac)
      (push '(menu-bar-lines . 0) default-frame-alist))
    (push '(tool-bar-lines . 0) default-frame-alist)
    (push '(vertical-scroll-bars) default-frame-alist))

  ;; Enable built-in trash support via finder API if available (only on Emacs
  ;; Mac Port)
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t))

  ;; Following statements sometimes doesn't run , so let it be called after user-config
  (spacemacs/defer-until-after-user-config
   (lambda ()
     (add-hook 'focus-in-hook 'memacs/autoescape-use-english-layout)))
  )

(defun osx/init-osx-trash ()
  (use-package osx-trash
    :if (and (spacemacs/system-is-mac)
           (not (boundp 'mac-system-move-file-to-trash-use-finder)))
    :init (osx-trash-setup)))
