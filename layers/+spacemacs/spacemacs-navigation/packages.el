;;; packages.el --- Spacemacs Navigation Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-navigation-packages
      '(ace-link
        ace-window
        auto-highlight-symbol
        centered-cursor-mode
        (compile :location built-in)
        (doc-view :location built-in)
        (grep :location built-in)
        (info+ :location local)
        open-junk-file
        paradox
        restart-emacs
        (smooth-scrolling :location built-in)))

(defun spacemacs-navigation/init-ace-link ()
  (use-package ace-link
    :defer t
    :commands spacemacs/ace-buffer-links
    :init
    (progn
      (define-key spacemacs-buffer-mode-map "o" 'spacemacs/ace-buffer-links)
      (with-eval-after-load 'info
        (define-key Info-mode-map "o" 'ace-link-info))
      (with-eval-after-load 'help-mode
        (define-key help-mode-map "o" 'ace-link-help)))))

(defun spacemacs-navigation/init-ace-window ()
  (use-package ace-window
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys
        "bD" 'spacemacs/ace-kill-this-buffer
        ;; FIXME: Needs new binding.
        ;; "wC" 'spacemacs/ace-center-window
        "wD" 'spacemacs/ace-delete-window
        "wM" 'ace-swap-window
        "wW" 'ace-window)
      ;; set ace-window keys to home-row
      (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))))

(defun spacemacs-navigation/init-auto-highlight-symbol ()
  (use-package auto-highlight-symbol
    :defer t
    :init
    (progn
      (setq ahs-case-fold-search nil
            ahs-default-range 'ahs-range-whole-buffer
            ;; by default disable auto-highlight of symbol
            ;; current symbol can always be highlighted with `SPC s h'
            ahs-idle-timer 0
            ahs-idle-interval 0.25
            ahs-inhibit-face-list nil
            spacemacs--symbol-highlight-transient-state-doc
            "
 %s  [_n_] next   [_N_/_p_] previous   [_r_] change range   [_R_] reset   [_e_] iedit
 %s  [_d_/_D_] next/previous definition")

      ;; since we are creating our own maps,
      ;; prevent the default keymap from getting created
      (setq auto-highlight-symbol-mode-map (make-sparse-keymap))

      (spacemacs|add-toggle automatic-symbol-highlight
        :status (timerp ahs-idle-timer)
        :on (progn
              (auto-highlight-symbol-mode)
              (setq ahs-idle-timer
                    (run-with-idle-timer ahs-idle-interval t
                                         'ahs-idle-function)))
        :off (when (timerp ahs-idle-timer)
               (auto-highlight-symbol-mode)
               (cancel-timer ahs-idle-timer)
               (setq ahs-idle-timer 0))
        :documentation "Automatic highlight of current symbol."
        :evil-leader "tha")
      (spacemacs/add-to-hooks 'auto-highlight-symbol-mode '(prog-mode-hook
                                                            markdown-mode-hook)))
    :config
    (progn
      (spacemacs|hide-lighter auto-highlight-symbol-mode)
      (defvar-local spacemacs-last-ahs-highlight-p nil
        "Info on the last searched highlighted symbol.")
      (defvar-local spacemacs--ahs-searching-forward t)

      (with-eval-after-load 'evil
        (define-key evil-motion-state-map (kbd "*")
          'spacemacs/enter-ahs-forward)
        (define-key evil-motion-state-map (kbd "#")
          'spacemacs/enter-ahs-backward))

      (define-key evil-normal-state-map (kbd "gH") 'spacemacs/goto-last-searched-ahs-symbol)

      ;; micro-state to easily jump from a highlighted symbol to the others
      (dolist (sym '(ahs-forward
                     ahs-forward-definition
                     ahs-backward
                     ahs-backward-definition
                     ahs-back-to-start
                     ahs-change-range))
        (let* ((advice (intern (format "spacemacs/%s" (symbol-name sym)))))
          (eval `(defadvice ,sym (around ,advice activate)
                   (spacemacs/ahs-highlight-now-wrapper)
                   ad-do-it
                   (spacemacs/ahs-highlight-now-wrapper)
                   (setq spacemacs-last-ahs-highlight-p (ahs-highlight-p))))))

      ;; transient state
      (spacemacs|define-transient-state symbol-highlight
        :title "Symbol Highlight Transient State"
        :dynamic-hint (spacemacs//symbol-highlight-ts-doc)
        :before-exit (spacemacs//ahs-ms-on-exit)
        :bindings
        ("d" ahs-forward-definition)
        ("D" ahs-backward-definition)
        ("e" spacemacs/ahs-to-iedit :exit t)
        ("n" spacemacs/quick-ahs-forward)
        ("N" spacemacs/quick-ahs-backward)
        ("p" spacemacs/quick-ahs-backward)
        ("R" ahs-back-to-start)
        ("r" ahs-change-range)
        ("q" nil :exit t)))))

(defun spacemacs-navigation/init-centered-cursor-mode ()
  (use-package centered-cursor-mode
    :defer t
    :commands (centered-cursor-mode
               global-centered-cursor-mode)
    :init
    (progn
      (spacemacs|add-toggle centered-point
        :mode centered-cursor-mode
        :documentation
        "Keep point at the center of the window."
        :evil-leader "t-")
      (spacemacs|add-toggle centered-point-globally
        :mode global-centered-cursor-mode
        :documentation
        "Keep point at the center of the window globally."
        :evil-leader "t C--"))
    :config
    (progn
      (setq ccm-recenter-at-end-of-file t
            ccm-ignored-commands '(mouse-drag-region
                                   mouse-set-point
                                   widget-button-click
                                   scroll-bar-toolkit-scroll
                                   evil-mouse-drag-region))
      (spacemacs|diminish centered-cursor-mode " ⊝" " -"))))

(defun spacemacs-navigation/init-compile ()
  (use-package compile
    :defer t
    :config
    (define-key compilation-mode-map "h" nil)))

(defun spacemacs-navigation/init-doc-view ()
  (use-package doc-view
    :defer t
    :init
    (evilified-state-evilify doc-view-mode doc-view-mode-map
      "/"  'spacemacs/doc-view-search-new-query
      "?"  'spacemacs/doc-view-search-new-query-backward
      "gg" 'doc-view-first-page
      "G"  'spacemacs/doc-view-goto-page
      "gt" 'doc-view-goto-page
      "h"  'doc-view-previous-page
      "j"  'doc-view-next-line-or-next-page
      "k"  'doc-view-previous-line-or-previous-page
      "K"  'doc-view-kill-proc-and-buffer
      "l"  'doc-view-next-page
      "n"  'doc-view-search
      "N"  'doc-view-search-backward
      (kbd "C-d") 'doc-view-scroll-up-or-next-page
      (kbd "C-k") 'doc-view-kill-proc
      (kbd "C-u") 'doc-view-scroll-down-or-previous-page)
    ))

(defun spacemacs-navigation/init-grep ()
  (use-package grep
    :defer t
    :config
    (define-key grep-mode-map "h" nil)))

(defun spacemacs-navigation/init-info+ ()
  (use-package info+
    :defer t
    :init
    (progn
      (with-eval-after-load 'info
        (require 'info+))
      (setq Info-fontify-angle-bracketed-flag nil))))

(defun spacemacs-navigation/init-open-junk-file ()
  (use-package open-junk-file
    :defer t
    :commands (open-junk-file)
    :init
    (progn
      (setq open-junk-file-format (concat spacemacs-cache-directory "junk/%Y/%m/%d-%H%M%S."))
      (spacemacs/set-leader-keys "fJ" 'spacemacs/open-junk-file))))

(defun spacemacs-navigation/init-paradox ()
  (use-package paradox
    :defer t
    :commands paradox-list-packages
    :init
    (progn
      (setq paradox-execute-asynchronously nil)
      (evilified-state-evilify paradox-menu-mode paradox-menu-mode-map
        "H" 'paradox-menu-quick-help
        "J" 'paradox-next-describe
        "K" 'paradox-previous-describe
        "L" 'paradox-menu-view-commit-list
        "o" 'paradox-menu-visit-homepage)
      (spacemacs/set-leader-keys
        "ak" 'spacemacs/paradox-list-packages))))

(defun spacemacs-navigation/init-restart-emacs ()
  (use-package restart-emacs
    :defer t
    :init
    (spacemacs/set-leader-keys
      "qd" 'spacemacs/restart-emacs-debug-init
      "qD" 'spacemacs/restart-stock-emacs-with-packages
      "qr" 'spacemacs/restart-emacs-resume-layouts
      "qR" 'spacemacs/restart-emacs
      "qt" 'spacemacs/restart-emacs-timed-requires
      "qt" 'spacemacs/restart-emacs-adv-timers)))

(defun spacemacs-navigation/init-smooth-scrolling ()
  (setq scroll-preserve-screen-position t
        scroll-margin 0
        scroll-conservatively (if dotspacemacs-smooth-scrolling 101 0))
  (spacemacs|add-toggle smooth-scrolling
    :status (= 101 scroll-conservatively)
    :on (spacemacs/enable-smooth-scrolling)
    :off (spacemacs/disable-smooth-scrolling)
    :documentation "Smooth scrolling."
    :evil-leader "tv"))
