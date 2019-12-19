;;; packages.el --- Spacemacs Evil Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-evil-packages
      '(
        evil-anzu
        evil-args
        evil-ediff
        evil-escape
        evil-exchange
        evil-goggles
        evil-iedit-state
        evil-indent-plus
        evil-lion
        evil-lisp-state
        evil-nerd-commenter
        evil-matchit
        evil-numbers
        evil-surround
        (evil-unimpaired :location (recipe :fetcher local))
        evil-visual-mark-mode
        evil-visualstar
        (hs-minor-mode :location built-in)
        vi-tilde-fringe
        eldoc))

(defun spacemacs-evil/init-evil-anzu ()
  (use-package evil-anzu
    :init
    (global-anzu-mode t)
    :config
    (progn
      (spacemacs|hide-lighter anzu-mode)
      (setq anzu-search-threshold 1000
            anzu-cons-mode-line-p nil)
      )))

(defun spacemacs-evil/post-init-eldoc ()
  (eldoc-add-command #'evil-cp-insert)
  (eldoc-add-command #'evil-cp-insert-at-end-of-form)
  (eldoc-add-command #'evil-cp-insert-at-beginning-of-form)
  (eldoc-add-command #'evil-cp-append))

(defun spacemacs-evil/init-evil-args ()
  (use-package evil-args
    :defer t
    :init
    (progn
      ;; bind evil-args text objects
      (define-key evil-inner-text-objects-map "a" 'evil-inner-arg)
      (define-key evil-outer-text-objects-map "a" 'evil-outer-arg))))

(defun spacemacs-evil/init-evil-ediff ()
  (use-package evil-ediff
    :after (ediff)))

(defun spacemacs-evil/init-evil-escape ()
  (use-package evil-escape
    :defer t
    :init
    (spacemacs//evil-escape-deactivate-in-holy-mode 'hybrid)
    :config (spacemacs|hide-lighter evil-escape-mode)))

(defun spacemacs-evil/init-evil-exchange ()
  (use-package evil-exchange
    :defer t
    :init
    (progn
      (let ((evil-exchange-key (kbd "gx"))
            (evil-exchange-cancel-key (kbd "gX")))
        (define-key evil-normal-state-map evil-exchange-key 'evil-exchange)
        (define-key evil-visual-state-map evil-exchange-key 'evil-exchange)
        (define-key evil-normal-state-map evil-exchange-cancel-key
          'evil-exchange-cancel)
        (define-key evil-visual-state-map evil-exchange-cancel-key
          'evil-exchange-cancel)))))

(defun spacemacs-evil/init-evil-goggles ()
  (use-package evil-goggles
    :defer t
    :init
    (progn
      ;; disable pulses as it is more distracting than useful and
      ;; less readable.
      (setq evil-goggles-pulse nil
            evil-goggles-async-duration 0.1
            evil-goggles-blocking-duration 0.05)
      (when (or vim-style-visual-feedback
              hybrid-style-visual-feedback)
        (spacemacs|add-transient-hook evil-operator-state-entry-hook
          (lambda () (require 'evil-goggles))
          lazy-load-evil-googles)))
    :config
    (progn
      (if (or vim-style-visual-feedback
              hybrid-style-visual-feedback)
          (evil-goggles-mode)
        (evil-goggles-mode -1))
      (spacemacs|hide-lighter evil-goggles-mode))))

(defun spacemacs-evil/init-evil-iedit-state ()
  (use-package evil-iedit-state
    :defer t
    :commands (evil-iedit-state evil-iedit-state/iedit-mode)
    :init
    (progn
      (setq iedit-current-symbol-default t
            iedit-only-at-symbol-boundaries t
            iedit-toggle-key-default nil))
    :config
    ;; activate leader in iedit and iedit-insert states
    (define-key evil-iedit-state-map
      (kbd dotspacemacs-leader-key) spacemacs-default-map)
    (spacemacs//iedit-insert-state-hybrid 'hybrid)))

(defun spacemacs-evil/init-evil-indent-plus ()
  (use-package evil-indent-plus
    :defer t
    :init
    (progn
      (define-key evil-inner-text-objects-map "i" 'evil-indent-plus-i-indent)
      (define-key evil-outer-text-objects-map "i" 'evil-indent-plus-a-indent)
      (define-key evil-inner-text-objects-map "I" 'evil-indent-plus-i-indent-up)
      (define-key evil-outer-text-objects-map "I" 'evil-indent-plus-a-indent-up)
      (define-key evil-inner-text-objects-map "J"
        'evil-indent-plus-i-indent-up-down)
      (define-key evil-outer-text-objects-map "J"
        'evil-indent-plus-a-indent-up-down))))

(defun spacemacs-evil/init-evil-lion ()
  (use-package evil-lion
    :defer t
    :init
    (progn
      ;; Override the default keys, as they collide (with what ? :-))
      (setq evil-lion-left-align-key nil
            evil-lion-right-align-key nil))
    :config (evil-lion-mode)))

(defun spacemacs-evil/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'spacemacs//load-evil-lisp-state)
      (setq evil-lisp-state-global t))
    :config (spacemacs/set-leader-keys "k" evil-lisp-state-map)))

;; other commenting functions in funcs.el with keybinds in keybindings.el
(defun spacemacs-evil/init-evil-nerd-commenter ()
  (use-package evil-nerd-commenter
    :defer t
    :commands (evilnc-comment-operator
               evilnc-copy-and-comment-lines
               evilnc-comment-or-uncomment-paragraphs)
    ))

(defun spacemacs-evil/init-evil-matchit ()
  (use-package evil-matchit
    :defer t))

(defun spacemacs-evil/init-evil-numbers ()
  (use-package evil-numbers
    :defer t))

(defun spacemacs-evil/init-evil-surround ()
  (use-package evil-surround
    :defer t
    :init
    (global-evil-surround-mode 1)))

(defun spacemacs-evil/init-evil-unimpaired ()
  ;; No laziness here, unimpaired bindings should be available right away.
  (use-package evil-unimpaired))

(defun spacemacs-evil/init-evil-visual-mark-mode ()
  (use-package evil-visual-mark-mode
    :defer t))

(defun spacemacs-evil/init-evil-visualstar ()
  (use-package evil-visualstar
    :commands (evil-visualstar/begin-search-forward
               evil-visualstar/begin-search-backward)
    :init
    (progn
      (define-key evil-visual-state-map (kbd "*")
        'evil-visualstar/begin-search-forward)
      (define-key evil-visual-state-map (kbd "#")
        'evil-visualstar/begin-search-backward))))

(defun spacemacs-evil/init-hs-minor-mode ()
  (add-hook 'prog-mode-hook 'spacemacs//enable-hs-minor-mode))

(defun spacemacs-evil/init-vi-tilde-fringe ()
  (spacemacs|do-after-display-system-init
   (use-package vi-tilde-fringe
     :init
     (progn
       (global-vi-tilde-fringe-mode)
       ;; don't enable it on some special buffers
       (with-current-buffer spacemacs-buffer-name
         (spacemacs/disable-vi-tilde-fringe))
       (add-hook 'which-key-init-buffer-hook 'spacemacs/disable-vi-tilde-fringe)
       ;; after a major mode is loaded, check if the buffer is read only
       ;; if so, disable vi-tilde-fringe-mode
       (add-hook 'after-change-major-mode-hook
                 'spacemacs/disable-vi-tilde-fringe-read-only)
       ;; TODO move this hook if/when we have a layer for eww
       ;; (spacemacs/add-to-hooks 'spacemacs/disable-vi-tilde-fringe
       ;;                         '(eww-mode-hook))
       )
     :config
     (spacemacs|hide-lighter vi-tilde-fringe-mode))))
