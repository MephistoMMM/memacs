;;; packages.el --- Spacemacs Misc. Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-misc-packages
      '(
        devdocs
        dumb-jump
        ))

(defun spacemacs-misc/init-dumb-jump ()
  (use-package dumb-jump
    :defer t
    :init
    (progn
      ;; not activating `dumb-jump-mode' because it only adds key bindings, and
      ;; they conflict with existing bindings (see
      ;; https://github.com/syl20bnr/spacemacs/issues/7107)

      (spacemacs/set-leader-keys "jq" #'dumb-jump-quick-look)

      (setq dumb-jump-selector 'ivy)

      ;; Since it's dumb, we add it to the end of the default jump handlers. At
      ;; the time of writing it is the only default jump handler. (gtags remains
      ;; mode-local)
      (add-to-list 'spacemacs-default-jump-handlers 'dumb-jump-go 'append))))

(defun spacemacs-misc/init-devdocs ()
  (use-package devdocs
    :defer t
    :init
    (progn
      (defalias 'spacemacs/browse-docs-online-at-point 'devdocs-search)
      (spacemacs/set-leader-keys "db" #'spacemacs/browse-docs-online-at-point))))
