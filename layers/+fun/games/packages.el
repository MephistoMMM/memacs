;;; packages.el --- games Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq games-packages
      '(
        2048-game
        ))

(defun games/init-2048-game ()
  (use-package 2048-mode
    :defer t
    :init
    (progn
      (evilified-state-evilify 2048-mode 2048-mode-map
        "j" '2048-down
        "k" '2048-up
        "h" '2048-left
        "l" '2048-right))))
