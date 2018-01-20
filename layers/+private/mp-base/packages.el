;;; mp-base/packages.el --- defines packages for base

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: base packages spacemacs elisp

;;; Commentary:

;; description

;;; Code:

(defconst mp-base-packages
  '(
    spaceline
    outshine
    )
 )

(defun mp-base/init-outshine ()
  "Bind outshine to SPE o o"
  (use-package outshine
    :defer t
    :init
    (spacemacs|diminish outline-minor-mode " ♗" " @")
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
    (add-hook 'prog-mode-hook 'outline-minor-mode)
    (advice-add 'outshine-narrow-to-subtree :before
                (lambda (&rest args) (unless (outline-on-heading-p t)
                                       (outline-previous-visible-heading 1))))

    ;; declare onwer prifix
    (spacemacs/declare-prefix "o" "owner")
    (spacemacs/declare-prefix "oo" "outshine")
    ;; Keybinding
    (spacemacs/set-leader-keys
      ;; Insert
      "ooi" 'outshine-insert-heading
      "oob" 'outshine-cycle-buffer

      ;; Narrowing
      "oon" 'outshine-narrow-to-subtree
      "oow" 'widen

      ;; Structural edits and moves
      "ooj" 'outline-forward-same-level
      "ook" 'outline-backward-same-level
      "ooh" 'outline-up-heading
      "ool" 'outline-next-visible-heading
      "oou" 'outline-previous-visible-heading
      "ooJ" 'outline-move-subtree-down
      "ooK" 'outline-move-subtree-up
      "ooH" 'outline-promote
      "ooL" 'outline-demote
      )
    )
  )

(defun mp-base/init-spaceline ()
  (use-package spaceline-config
    :if (memq (spacemacs/get-mode-line-theme-name) '(spacemacs))
    :init
    (progn
      (add-hook 'spacemacs-post-user-config-hook 'mp-base/spaceline-compile)
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
      ;; Additional spacelines
      (when (package-installed-p 'helm)
        (spaceline-helm-mode t))
      (when (configuration-layer/package-used-p 'info+)
        (spaceline-info-mode t))
      ;; Enable spaceline for buffers created before the configuration of
      ;; spaceline
      (spacemacs//set-powerline-for-startup-buffers))))
;;; mp-base/packages.el ends here
