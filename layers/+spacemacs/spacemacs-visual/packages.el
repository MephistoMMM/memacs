;;; packages.el --- Spacemacs UI Visual Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-visual-packages
      '(
        ;; (ansi-colors :location built-in)
        desktop
        ;; `display-fill-column-indicator' is available in Emacs 27+
        (display-fill-column-indicator :location built-in
                                       :toggle (boundp 'display-fill-column-indicator))
        (fill-column-indicator :toggle (not (boundp 'display-fill-column-indicator)))
        hl-todo
        popup
        popwin
        (posframe :location (recipe
                             :fetcher github
                             :repo "tumashu/posframe"))
        (company-posframe :location (recipe
                                     :fetcher github
                                     :repo "tumashu/company-posframe"))
        ))

;; (defun spacemacs-visual/init-ansi-colors ()
;;   (add-hook 'compilation-filter-hook
;;             'spacemacs-visual//compilation-buffer-apply-ansi-colors))

(defun spacemacs-visual/init-posframe ())

(defun spacemacs-visual/init-company-posframe ()
  (use-package company-posframe
    :defer t
    :config (spacemacs|diminish company-posframe-mode)))

(defun spacemacs-visual/init-desktop ()
  (use-package desktop
    :defer t
    :init
    (setq desktop-dirname spacemacs-cache-directory)
    :config
    (progn
      (add-to-list 'desktop-path spacemacs-cache-directory)
      (push '(company-posframe-mode . nil)
            desktop-minor-mode-table)
      )))

(defun spacemacs-visual/init-display-fill-column-indicator ()
  (spacemacs|add-toggle fill-column-indicator
    :mode display-fill-column-indicator-mode
    :documentation "Display the fill column indicator."
    :evil-leader "tf")
  (spacemacs|add-toggle fill-column-indicator-globally
    :mode global-display-fill-column-indicator-mode
    :documentation "Display the fill column indicator globally."
    :evil-leader "t C-f")
  (with-eval-after-load 'display-fill-column-indicator
    ;; manually register the minor mode since it does not define any
    ;; lighter
    (add-to-list 'minor-mode-alist '(display-fill-column-indicator-mode ""))
    (spacemacs|diminish display-fill-column-indicator-mode " ⓕ" " f")))

(defun spacemacs-visual/init-fill-column-indicator ()
  (use-package fill-column-indicator
    :defer t
    :init
    (progn
      (setq fci-rule-width 1)
      ;; manually register the minor mode since it does not define any
      ;; lighter
      (add-to-list 'minor-mode-alist '(fci-mode ""))
      (spacemacs|add-toggle fill-column-indicator
        :status fci-mode
        :on (turn-on-fci-mode)
        :off (turn-off-fci-mode)
        :documentation "Display the fill column indicator."
        :evil-leader "tf"))
    :config
    (spacemacs|diminish fci-mode " ⓕ" " f")))

(defun spacemacs-visual/init-hl-todo ()
  (use-package hl-todo
    :defer t
    :init
    ;; global hook activates hl-todo-mode for prog-mode, text-mode
    ;; mode can be explicitly defined using hl-todo-activate-in-modes variable
    (global-hl-todo-mode 1)))

(defun spacemacs-visual/init-popup ()
  (use-package popup
    :defer t
    :commands (popup-menu*)))

(defun spacemacs-visual/init-popwin ()
  (use-package popwin
    :config
    (progn
      (popwin-mode 1)
      (spacemacs/set-leader-keys "em" 'popwin:messages)
      (spacemacs/set-leader-keys "ed" 'popwin:close-popup-window)

      ;; don't use default value but manage it ourselves
      (setq popwin:special-display-config nil)

      ;; buffers that we manage
      (push '("*Help*"                 :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Process List*"         :dedicated t :position bottom :stick t :noselect nil :height 0.4) popwin:special-display-config)
      (push '("*compilation*"          :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config)
      (push '("*Shell Command Output*" :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*Async Shell Command*"  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*undo-tree*"            :dedicated t :position right  :stick t :noselect nil :width   60) popwin:special-display-config)
      (push '("*undo-tree Diff*"       :dedicated t :position bottom :stick t :noselect nil :height 0.3) popwin:special-display-config)
      (push '("*ert*"                  :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*grep*"                 :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("*nosetests*"            :dedicated t :position bottom :stick t :noselect nil            ) popwin:special-display-config)
      (push '("^\*WoMan.+\*$" :regexp t             :position bottom                                   ) popwin:special-display-config)
      (push '("*Google Translate*"     :dedicated t :position bottom :stick t :noselect t   :height 0.4) popwin:special-display-config))))

;; TODO: Create a magnifier like skim magnifier
