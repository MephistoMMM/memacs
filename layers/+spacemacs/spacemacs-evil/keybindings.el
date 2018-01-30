;;; keybindings.el --- Spacemacs-evil Layer Keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; iedit
(define-key evil-normal-state-map "gI" 'evil-iedit-state/iedit-mode)

;;; lion
(spacemacs/set-leader-keys
  "xal" 'evil-lion-left
  "xaL" 'evil-lion-right)

;;; lisp state
(spacemacs/set-leader-keys "L" evil-lisp-state-map)

;;; mc
(define-key evil-normal-state-map "p" 'spacemacs/evil-mc-paste-after)
(define-key evil-normal-state-map "P" 'spacemacs/evil-mc-paste-before)

;;; nerd commenter
(define-key evil-normal-state-map "gc" 'evilnc-comment-operator)
(define-key evil-normal-state-map "gp" 'spacemacs/comment-or-uncomment-paragraphs)
(define-key evil-normal-state-map "gy" 'spacemacs/copy-and-comment-lines)

;;; numbers
(spacemacs|define-transient-state evil-numbers
  :title "Evil Numbers Transient State"
  :doc
  "\n[_+_/_=_] increase number  [_-_] decrease  [0..9] prefix  [_q_] quit"
  :bindings
  ("+" evil-numbers/inc-at-pt)
  ("=" evil-numbers/inc-at-pt)
  ("-" evil-numbers/dec-at-pt)
  ("q" nil :exit t))
(spacemacs/set-leader-keys
  "+" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
  "=" 'spacemacs/evil-numbers-transient-state/evil-numbers/inc-at-pt
  "-" 'spacemacs/evil-numbers-transient-state/evil-numbers/dec-at-pt)

;;; search highlight
(memacs/define-evil-normal-keybinding "gs" 'spacemacs/evil-search-clear-highlight)

;;; surround
;; `s' for surround instead of `substitute'
;; see motivation for this change in the documentation
(evil-define-key 'visual evil-surround-mode-map "s" 'evil-surround-region)
(evil-define-key 'visual evil-surround-mode-map "S" 'evil-substitute)

;;; visual mark
(spacemacs|add-toggle evil-visual-mark-mode
  :mode evil-visual-mark-mode
  :documentation "Enable evil visual marks mode."
  :evil-leader "t`")

;;; linum relative
(spacemacs/set-leader-keys "tr" 'spacemacs/linum-relative-toggle)

;;; vi tilde fringe
(spacemacs|add-toggle vi-tilde-fringe
  :mode global-vi-tilde-fringe-mode
  :documentation
  "Globally display a ~ on empty lines in the fringe."
  :evil-leader "T~")
