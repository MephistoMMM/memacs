;;; config.el --- dired Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2014 Sylvain Benner
;; Copyright (c) 2014-2016 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Make dired "guess" target directory for some operations, like copy to
;; directory visited in other split buffer.
(setq dired-dwim-target t)

(defvar memacs-dired-omit nil
  "If dired-omit-mode enabled by default. Don't setq me.")

(defvar memacs-dired-sort 'name)

(defvar ranger-enter-with-minus t
  "Option to enter `deer' when `-' is pressed.  Idea from `vim-vinegar'.")
