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
    :if (memq (spacemacs/get-mode-line-theme-name) '(spacemacs))
    :init
    (progn
      (add-hook 'spacemacs-post-user-config-hook 'memacs/spaceline-compile)
      (add-hook 'spacemacs-post-theme-change-hook
                'spacemacs/customize-powerline-faces)
      (add-hook 'spacemacs-post-theme-change-hook 'powerline-reset)
      (setq powerline-default-separator (or (spacemacs/mode-line-separator) 'wave)
            powerline-scale (or (spacemacs/mode-line-separator-scale) 1.5)
            powerline-height (spacemacs/compute-mode-line-height))
      (spacemacs|do-after-display-system-init
       ;; seems to be needed to avoid weird graphical artefacts with the
       ;; first graphical client
       (require 'spaceline)))
    :config
    (progn
      (spacemacs/customize-powerline-faces)
      (setq spaceline-org-clock-p nil
            spaceline-highlight-face-func 'spacemacs//evil-state-face)
      ;; Segment toggles
      (dolist (spec '((minor-modes "tmm")
                      (major-mode "tmM")
                      (version-control "tmv")
                      (new-version "tmV")
                      (point-position "tmp")
                      (org-clock "tmc")))
        (let* ((segment (car spec))
               (status-var (intern (format "spaceline-%S-p" segment))))
          (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
                   :status ,status-var
                   :on (setq ,status-var t)
                   :off (setq ,status-var nil)
                   :documentation ,(format "Show %s in the mode-line."
                                           (replace-regexp-in-string
                                            "-" " " (format "%S" segment)))
                   :evil-leader ,(cadr spec)))))
      ;; unicode
      (let ((unicodep (dotspacemacs|symbol-value
                       dotspacemacs-mode-line-unicode-symbols)))
        (setq spaceline-window-numbers-unicode unicodep
              spaceline-workspace-numbers-unicode unicodep))
      (add-hook 'spaceline-pre-hook 'spacemacs//prepare-diminish)
      (when (configuration-layer/package-used-p 'info+)
        (spaceline-info-mode t))
      ;; Enable spaceline for buffers created before the configuration of
      ;; spaceline
      (spacemacs//set-powerline-for-startup-buffers))))

;;; memacs/packages.el ends here
