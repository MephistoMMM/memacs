;; -*- lexical-binding: t -*-
;;
;;; packages.el --- Spacemacs Multiple Cursors Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <codrut.gusoi@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq multiple-cursors-packages
      '(
        evil-mc
        ))

(defun multiple-cursors/init-evil-mc ()
  (use-package evil-mc
    :defer t
    :init
    (progn
      (setq-default evil-mc-one-cursor-show-mode-line-text nil)

      ;; evil-mc is not compatible with the paste transient state
      (evil-define-key 'normal evil-mc-key-map
        "p" #'spacemacs/evil-mc-paste-after
        "P" #'spacemacs/evil-mc-paste-before)

      (evil-define-key '(normal insert) evil-mc-key-map
        (kbd "C-M-j") #'evil-mc-make-cursor-move-next-line
        (kbd "C-M-k") #'evil-mc-make-cursor-move-prev-line)

      (add-hook 'prog-mode-hook 'turn-on-evil-mc-mode)
      (add-hook 'text-mode-hook 'turn-on-evil-mc-mode))
    :config
    (progn
      (spacemacs|diminish evil-mc-mode)
      (when (spacemacs/system-is-mac)
        (setq evil-mc-enable-bar-cursor nil))

      (dolist (keydata '(("C-n" . nil )
                         ("C-t" . nil )
                         ("C-p" . nil)))
        (evil-define-key 'normal evil-mc-key-map (kbd (car keydata)) nil)
        (evil-define-key 'visual evil-mc-key-map (kbd (car keydata)) nil))
      ))
  )
