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
      '(evil-anzu
        evil-args
        evil-ediff
        evil-exchange
        evil-iedit-state
        evil-indent-plus
        evil-lion
        evil-lisp-state
        ;; for testing purpose, contribute by reporting bugs and sending PRs
        ;; to https://github.com/gabesoft/evil-mc
        ;; To enable it add `(global-evil-mc-mode)' to user-config function
        evil-mc
        evil-nerd-commenter
        evil-matchit
        evil-numbers
        evil-search-highlight-persist
        evil-surround
        (evil-unimpaired :location (recipe :fetcher local))
        evil-visual-mark-mode
        (hs-minor-mode :location built-in)
        linum-relative
        vi-tilde-fringe
        ))

(defun spacemacs-evil/init-evil-anzu ()
  (use-package evil-anzu
    :init
    (global-anzu-mode t)
    :config
    (progn
      (spacemacs|hide-lighter anzu-mode)
      (setq anzu-search-threshold 1000
            anzu-cons-mode-line-p nil)
      ;; powerline integration
      (when (configuration-layer/package-used-p 'spaceline)
        (defun spacemacs/anzu-update-mode-line (here total)
          "Custom update function which does not propertize the status."
          (when anzu--state
            (let ((status
                   (cl-case anzu--state
                     (search (format "(%s/%d%s)"
                                     (anzu--format-here-position here total)
                                     total (if anzu--overflow-p "+" "")))
                     (replace-query (format "(%d replace)" total))
                     (replace (format "(%d/%d)" here total)))))
              status)))
        (setq anzu-mode-line-update-function
              'spacemacs/anzu-update-mode-line)))))

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

(defun spacemacs-evil/init-evil-exchange ()
  (use-package evil-exchange
    :init (evil-exchange-install)))

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
    :init (evil-indent-plus-default-bindings)))

(defun spacemacs-evil/init-evil-lion ()
  (use-package evil-lion
    :init
    (progn
      ;; Override the default keys, as they collide
      (setq evil-lion-left-align-key nil
            evil-lion-right-align-key nil)
      (evil-lion-mode))))

(defun spacemacs-evil/init-evil-lisp-state ()
  (use-package evil-lisp-state
    :init (setq evil-lisp-state-global t)))

(defun spacemacs-evil/init-evil-mc ()
  (use-package evil-mc
    :defer t
    :init
    (progn
      ;; evil-mc is not compatible with the paste transient state
      (setq evil-mc-one-cursor-show-mode-line-text nil)
      (when (or (spacemacs/system-is-mac) (spacemacs/system-is-mswindows))
        (setq evil-mc-enable-bar-cursor nil)))))

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

(defun spacemacs-evil/init-evil-search-highlight-persist ()
  (use-package evil-search-highlight-persist
    :init
    (progn
      (global-evil-search-highlight-persist)
      (evil-ex-define-cmd "nohlsearch" 'spacemacs/evil-search-clear-highlight)
      (spacemacs//adaptive-evil-highlight-persist-face)
      (add-hook 'spacemacs-post-theme-change-hook
                'spacemacs//adaptive-evil-highlight-persist-face))))

(defun spacemacs-evil/init-evil-surround ()
  (use-package evil-surround
    :init
    (global-evil-surround-mode 1)))

(defun spacemacs-evil/init-evil-unimpaired ()
  ;; No laziness here, unimpaired bindings should be available right away.
  (use-package evil-unimpaired))

(defun spacemacs-evil/init-evil-visual-mark-mode ()
  (use-package evil-visual-mark-mode
    :defer t))

(defun spacemacs-evil/init-hs-minor-mode ()
  (add-hook 'prog-mode-hook 'spacemacs//enable-hs-minor-mode))

(defun spacemacs-evil/init-linum-relative ()
  (use-package linum-relative
    :commands (linum-relative-toggle linum-relative-on)
    :init
    (when (or (eq dotspacemacs-line-numbers 'relative)
              (and (listp dotspacemacs-line-numbers)
                    (car (spacemacs/mplist-get dotspacemacs-line-numbers
                                              :relative))))
      (add-hook 'spacemacs-post-user-config-hook 'linum-relative-on))
    :config
    (setq linum-relative-current-symbol "")))

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
