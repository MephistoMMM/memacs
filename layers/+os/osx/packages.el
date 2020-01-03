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

(defconst osx-packages
  '(
    exec-path-from-shell
    launchctl
    osx-trash
    (spotlight :location local)
    ))

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
  ;; macOS Port)
  (when (boundp 'mac-system-move-file-to-trash-use-finder)
    (setq mac-system-move-file-to-trash-use-finder t))

  ;; Following statements sometimes doesn't run , so let it be called after user-config
  (spacemacs/defer-until-after-user-config
   (lambda ()
     (add-hook 'focus-in-hook 'memacs/autoescape-use-english-layout)))
  )

(defun osx/init-exec-path-from-shell ()
  (use-package exec-path-from-shell
    :if (spacemacs/system-is-mac)
    :config
    (progn
      (exec-path-from-shell-initialize)
      ;; Use GNU ls as `gls' from `coreutils' if available.  Add `(setq
      ;; dired-use-ls-dired nil)' to your config to suppress the Dired warning when
      ;; not using GNU ls.  We must look for `gls' after `exec-path-from-shell' was
      ;; initialized to make sure that `gls' is in `exec-path'
      (let ((gls (executable-find "gls")))
        (when gls
          (setq insert-directory-program gls
                dired-listing-switches "-aBhl --group-directories-first"))))))

(defun osx/init-launchctl ()
  (use-package launchctl
    :if (spacemacs/system-is-mac)
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.plist\\'" . nxml-mode))
      (spacemacs/set-leader-keys "al" 'launchctl))
    :config
    (progn
      (evilified-state-evilify launchctl-mode launchctl-mode-map
        (kbd "q") 'quit-window
        (kbd "s") 'tabulated-list-sort
        (kbd "g") 'launchctl-refresh
        (kbd "n") 'launchctl-new
        (kbd "e") 'launchctl-edit
        (kbd "v") 'launchctl-view
        (kbd "l") 'launchctl-load
        (kbd "u") 'launchctl-unload
        (kbd "r") 'launchctl-reload
        (kbd "S") 'launchctl-start
        (kbd "K") 'launchctl-stop
        (kbd "R") 'launchctl-restart
        (kbd "D") 'launchctl-remove
        (kbd "d") 'launchctl-disable
        (kbd "E") 'launchctl-enable
        (kbd "i") 'launchctl-info
        (kbd "f") 'launchctl-filter
        (kbd "=") 'launchctl-setenv
        (kbd "#") 'launchctl-unsetenv
        (kbd "h") 'launchctl-help))))

(defun osx/init-osx-trash ()
  (use-package osx-trash
    :if (and (spacemacs/system-is-mac)
           (not (boundp 'mac-system-move-file-to-trash-use-finder)))
    :init (osx-trash-setup)))

(defun osx/init-spotlight ()
  (use-package spotlight
    :if (spacemacs/system-is-mac)
    :commands (spotlight-fast spotlight)
    :defer t))
