;;; packages.el --- ranger Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Rich Alesi
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ranger-packages
      '(ranger))

(defun ranger/init-ranger ()
  (use-package ranger
    :defer t
    :commands (ranger)
    :init
    (progn
      ;; allow '-' to enter ranger
      (when ranger-enter-with-minus
        (define-key evil-normal-state-map (kbd "-") 'deer))

      ;; set up image-dired to allow picture resize
      (setq image-dired-dir (concat spacemacs-cache-directory "image-dir"))

      (setq ranger-ignored-extensions
            '("mkv" "iso" "mp4" "flv" "jpg" "png")
            ranger-max-preview-size 2)
      (unless (file-directory-p image-dired-dir)
        (make-directory image-dired-dir)))
    :config
    (define-key ranger-mode-map (kbd "-") 'ranger-up-directory)))
