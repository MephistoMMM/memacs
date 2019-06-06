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
      '(
        anzu
        spaceline
        doom-modeline
        fancy-battery))

(defun spacemacs-modeline/post-init-anzu ()
  (when (eq 'all-the-icons (spacemacs/get-mode-line-theme-name))
    (spaceline-all-the-icons--setup-anzu)))

(defun spacemacs-modeline/init-doom-modeline ()
  (use-package doom-modeline
    :defer t
    :if (eq (spacemacs/get-mode-line-theme-name) 'doom)
    :init (doom-modeline-init)))

(defun spacemacs-modeline/init-fancy-battery ()
  (use-package fancy-battery
    :defer t
    :init
    (progn
      (spacemacs|add-toggle mode-line-battery
        :mode fancy-battery-mode
        :documentation "Display battery info in mode-line."
        :evil-leader "tmb")
      (setq-default fancy-battery-show-percentage t))))

;; TODO: let the color of text in mode line be the same as that of evil state
(defun spacemacs-modeline/init-spaceline ()
  (use-package spaceline-config
    :if (memq (spacemacs/get-mode-line-theme-name) '(memacs))
    :init
    (progn
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (spacemacs|add-transient-hook window-configuration-change-hook
                    (lambda ()
                      (setq spaceline-byte-compile t)
                      ;; this must also be set in this hook because
                      ;; (spacemacs/compute-mode-line-height) returns incorrect
                      ;; results if it is called before the display system is
                      ;; initialized. see issue for details:
                      ;; https://github.com/syl20bnr/spacemacs/issues/10181
                      (setq powerline-height
                            (spacemacs/compute-mode-line-height))
                      (memacs/spaceline-compile))
                    lazy-load-spaceline)))
      (add-hook 'spacemacs-post-theme-change-hook 'powerline-reset)
      (setq powerline-default-separator (or (spacemacs/mode-line-separator) 'wave)
            powerline-image-apple-rgb (eq window-system 'ns)
            powerline-scale (or (spacemacs/mode-line-separator-scale) 1.5)
            spaceline-byte-compile nil))
    :config
    (progn
      (setq spaceline-org-clock-p nil
            spaceline-highlight-face-func 'spacemacs//evil-state-face)
      ;; unicode
      (let ((unicodep (dotspacemacs|symbol-value
                       dotspacemacs-mode-line-unicode-symbols)))
        (setq spaceline-workspace-numbers-unicode unicodep))
      (add-hook 'spaceline-pre-hook 'spacemacs//prepare-diminish)
      ;; this mode rely on info+ package
      (spaceline-info-mode t)
      ;; Enable spaceline for buffers created before the configuration of
      ;; spaceline
      (spacemacs//restore-buffers-powerline)

      (setq spaceline-byte-compile t)
      (memacs/spaceline-compile))))
