;;; packages.el --- Spacemacs Editing Visual Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq spacemacs-editing-visual-packages
      '(
        ;; default
        (hide-comnt :location local)
        column-enforce-mode
        highlight-indentation
        highlight-numbers
        highlight-parentheses
        volatile-highlights
        writeroom-mode
        ))

;; Initialization of packages

(defun spacemacs-editing-visual/init-writeroom-mode ()
  (use-package writeroom-mode
    :defer t
    :init
    (spacemacs|define-transient-state centered-buffer-mode
      :title "Centered Buffer Transient State"
      :bindings
      ("[" writeroom-decrease-width "shrink")
      ("]" writeroom-increase-width "enlarge")
      ("=" writeroom-adjust-width "adjust width"))))

(defun spacemacs-editing-visual/init-column-enforce-mode ()
  (use-package column-enforce-mode
    :defer t
    :commands (column-enforce-mode global-column-enforce-mode)
    :init
    (progn
      (spacemacs|add-toggle highlight-long-lines
        :status column-enforce-mode
        :prefix columns
        :on (column-enforce-n (or columns column-enforce-column))
        :on-message (format "long-lines enabled for %s columns."
                            (or columns column-enforce-column))
        :off (column-enforce-mode -1)
        :documentation "Highlight the characters past the 80th column."
        :evil-leader "t8")
      (spacemacs|add-toggle highlight-long-lines-globally
        :mode global-column-enforce-mode
        :documentation "Globally Highlight the characters past the 80th column."
        :evil-leader "t C-8"))
    :config (spacemacs|diminish column-enforce-mode "⑧" "8")))

(defun spacemacs-editing-visual/init-hide-comnt ()
  (use-package hide-comnt
    :commands hide/show-comments-toggle
    :init (define-key evil-normal-state-map (kbd "g C-h") 'hide/show-comments-toggle)
    ))

(defun spacemacs-editing-visual/init-highlight-indentation ()
  (use-package highlight-indentation
    :defer t
    :init
    (progn
      (spacemacs|add-toggle highlight-indentation
        :mode highlight-indentation-mode
        :documentation "Highlight indentation levels."
        :evil-leader "thi"))
    :config
    (progn
      (spacemacs|diminish highlight-indentation-mode " ⓗi" " hi")
      (spacemacs|diminish
       highlight-indentation-current-column-mode " ⓗc" " hc"))))

(defun spacemacs-editing-visual/init-highlight-numbers ()
  (use-package highlight-numbers
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'highlight-numbers-mode)
      (add-hook 'asm-mode-hook (lambda () (highlight-numbers-mode -1))))))

(defun spacemacs-editing-visual/init-highlight-parentheses ()
  (use-package highlight-parentheses
    :defer t
    :init
    (progn
      (when (member dotspacemacs-highlight-delimiters '(all current))
        (add-hook 'prog-mode-hook #'highlight-parentheses-mode))
      (setq hl-paren-delay 0.2)
      (spacemacs/set-leader-keys "thp" 'highlight-parentheses-mode)
      (setq hl-paren-colors '("#50fa7b"
                              "#0189cc"
                              "#bd93f9"
                              "#ff79c6"
                              "#ff5555"
                              "IndianRed3"
                              "IndianRed4")))
    :config
    (spacemacs|hide-lighter highlight-parentheses-mode)
    (set-face-attribute 'hl-paren-face nil :weight 'ultra-bold)))

(defun spacemacs-editing-visual/init-volatile-highlights ()
  (use-package volatile-highlights
    :defer (spacemacs/defer 2)
    :config
    (progn
      (require 'volatile-highlights)
      ;; additional extensions
      ;; evil
      (vhl/define-extension 'evil
                            'evil-move
                            'evil-paste-after
                            'evil-paste-before
                            'evil-paste-pop)
      (with-eval-after-load 'evil
        (vhl/install-extension 'evil)
        (vhl/load-extension 'evil))
      ;; undo-tree
      (vhl/define-extension 'undo-tree
                            'undo-tree-move
                            'undo-tree-yank)
      (with-eval-after-load 'undo-tree
        (vhl/install-extension 'undo-tree)
        (vhl/load-extension 'undo-tree))
      (volatile-highlights-mode)
      (spacemacs|hide-lighter volatile-highlights-mode))))
