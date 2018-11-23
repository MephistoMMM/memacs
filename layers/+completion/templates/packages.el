;;; packages.el --- Template Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq templates-packages '(yatemplate))

;; TODO design an approach to support local special template
(defun templates/init-yatemplate ()
  (use-package yatemplate
    :init
    (progn
      (setq yatemplate-dir (concat spacemacs-start-directory "templates/"))
      (unless templates-use-default-templates
        (setq auto-insert-alist nil)))
    :config
    (progn
      (yatemplate-fill-alist)
      (auto-insert-mode +1))))
