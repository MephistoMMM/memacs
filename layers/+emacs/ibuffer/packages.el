;;; packages.el --- ibuffer Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Aleksandr Guljajev <aleksandr.guljajev@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ibuffer-packages
      '(
        ibuffer
        ibuffer-projectile
        ))

(defun ibuffer/init-ibuffer()
  (use-package ibuffer
    :init
    (progn
      (add-hook 'ibuffer-hook
                'memacs//ibuffer-group-by-modes-or-projectile)

      ;; Use ibuffer to provide :ls
      (evil-ex-define-cmd "buffers" 'ibuffer))))

(defun ibuffer/init-ibuffer-projectile()
    (use-package ibuffer-projectile
      :defer t))
