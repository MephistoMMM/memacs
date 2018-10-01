;;; funcs.el --- Spacemacs UI Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; ansi-colors

(defun spacemacs-visual//compilation-buffer-apply-ansi-colors ()
  (let ((inhibit-read-only t))
    (goto-char compilation-filter-start)
    (ansi-color-apply-on-region (line-beginning-position) (point-max))))


;; popwin

(defun spacemacs/remove-popwin-display-config (str)
  "Removes the popwin display configurations that matches the passed STR"
  (setq popwin:special-display-config
        (-remove (lambda (x) (if (and (listp x) (stringp (car x)))
                                 (string-match str (car x))))
                 popwin:special-display-config)))

