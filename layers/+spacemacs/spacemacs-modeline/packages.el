;;; packages.el --- Spacemacs Mode-line Visual Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-modeline-packages
      '(doom-modeline))

(defun spacemacs-modeline/init-doom-modeline ()
  (use-package doom-modeline
    :defer t
    :if (eq (spacemacs/get-mode-line-theme-name) 'doom)
    :hook (after-init . doom-modeline-mode)
    :init
    (progn
      ;; prevent flash of unstyled modeline at startup
      (unless after-init-time
        (setq doom-modeline--old-format mode-line-format)
        (setq-default mode-line-format nil))
      (setq doom-modeline-major-mode-color-icon t
            doom-modeline-minor-modes nil
            doom-modeline-mu4e nil
            doom-modeline-buffer-file-name-style 'buffer-name
            ))
    ;; custom doom-line: https://github.com/seagle0128/doom-modeline
    ))
