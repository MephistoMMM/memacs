;;; packages.el --- dired Layer packages File for Spacemacs
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

(setq dired-packages
      '(ranger
        (dired :location built-in)
        (dired-x :location built-in)
        (dired+ :location local)
        ))

(defun dired/init-dired ()
  (add-hook 'dired-after-readin-hook 'memacs/dired-directory-sort)
  (add-hook 'dired-mode-hook (lambda ()
                               (memacs//dired-omit-auto-apply)
                               (memacs//dired-sort-auto-apply)))
  )

(defun dired/init-dired+ ()
  (use-package dired+
    :after dired-x
    :config
    (progn
      (setq
       diredp-hide-details-initially-flag nil
       diredp-hide-details-propagate-flag t
       diredp-hide-details-last-state nil)
      (setq dired-isearch-filenames t)

      ;; use KB MB show file size
      (setq dired-listing-switches "-alh")

      (setq find-name-arg "-iname")
      ;; fix up Chinese code bug in *Find*
      (setq find-ls-option '("-print0 | xargs -0 ls -ald" . ""))

      ;; copy files: never query
      (setq dired-recursive-copies  'always)
      ;; delete files: only query once
      (setq dired-recursive-deletes 'top)

      (define-key dired-mode-map (kbd "C-h") 'memacs/dired-backward)
      )
    ))

(defun dired/init-dired-x ()
  (use-package dired-x
    :defer t
    :commands (dired-jump
               dired-jump-other-window
               dired-omit-mode)))

(defun dired/init-ranger ()
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
