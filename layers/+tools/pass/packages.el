;;; packages.el --- Passwords Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Andrew Oppenlander <andrew.oppenlander@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq pass-packages
      '(
        (ivy-pass :requires ivy)
        password-store
        ))

(defun pass/init-ivy-pass ()
  (use-package ivy-pass
    :defer t
    :init
    (evil-leader/set-key
      "mp/" 'ivy-pass)))

(defun pass/init-password-store ()
  (use-package password-store
    :defer t
    :commands (password-store-list password-store--completing-read)
    :init
    (progn
      (spacemacs/declare-prefix "mp" "pass")
      (evil-leader/set-key
        "mpy" 'password-store-copy
        "mpg" 'password-store-generate
        "mpi" 'password-store-insert
        "mpc" 'password-store-edit
        "mpr" 'password-store-rename
        "mpd" 'password-store-remove
        "mpD" 'password-store-clear
        "mpI" 'password-store-init
        "mpw" 'password-store-url
        "mp?" 'spacemacs/pass-describe
        "mpY" 'spacemacs/pass-copy-and-describe))
    :config
    (when (and (configuration-layer/layer-usedp 'osx)
             (spacemacs/system-is-mac))
      (defun password-store-copy (entry)
        "Add password for ENTRY to kill ring.

Modify origin password-store-copy to call it in iterm2"
        (interactive (list (password-store--completing-read)))
        (condition-case nil
            (let ((password (password-store-get entry)))
              (password-store-clear)
              (kill-new password)
              (setq password-store-kill-ring-pointer kill-ring-yank-pointer)
              (message "Copied %s to the kill ring. Will clear in %s seconds."
                       entry (password-store-timeout))
              (setq password-store-timeout-timer
                    (run-at-time (password-store-timeout)
                                 nil 'password-store-clear)))
          (error
           (memacs//pass-store-copy entry)))
        )
      )
    ))
