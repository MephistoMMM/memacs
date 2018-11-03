;; -*- lexical-binding: t -*-
;;
;;; funcs.el --- Spacemacs Multiple Cursors Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Codruț Constantin Gușoi <codrut.gusoi@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs/evil-mc-paste-after (&optional count register)
  "Disable paste transient state if there is more than 1 cursor."
  (interactive "p")
  (setq this-command 'evil-paste-after)
  (evil-paste-after count (or register evil-this-register)))

(defun spacemacs/evil-mc-paste-before (&optional count register)
  "Disable paste transient state if there is more than 2 cursor."
  (interactive "p")
  (setq this-command 'evil-paste-before)
  (evil-paste-before count (or register evil-this-register)))
