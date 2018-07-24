;;; memacs/packages.el --- defines packages for base

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: base packages spacemacs elisp

;;; Commentary:

;; description

;;; Code:

(defconst memacs-packages
  '(
    spaceline
    )
 )

;; TODO: let the color of text in mode line be the same as that of evil state
(defun memacs/init-spaceline ()
  (use-package spaceline-config
    :if (memq (spacemacs/get-mode-line-theme-name) '(memacs))
    :init
    (progn
      (add-hook 'emacs-startup-hook
                (lambda ()
                  (spacemacs|add-transient-hook window-configuration-change-hook
                    (lambda ()
                      (setq spaceline-byte-compile t)
                      (memacs/spaceline-compile))
                    lazy-load-spaceline)))
      (add-hook 'spacemacs-post-theme-change-hook 'powerline-reset)
      (setq powerline-default-separator (or (spacemacs/mode-line-separator) 'wave)
            powerline-image-apple-rgb (eq window-system 'ns)
            powerline-scale (or (spacemacs/mode-line-separator-scale) 1.5)
            powerline-height (spacemacs/compute-mode-line-height)
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
      (memacs/spaceline-compile)
      ;; this mode rely on info+ package
      (spaceline-info-mode t)
      ;; Enable spaceline for buffers created before the configuration of
      ;; spaceline
      (spacemacs//restore-buffers-powerline))))

;;; memacs/packages.el ends here
