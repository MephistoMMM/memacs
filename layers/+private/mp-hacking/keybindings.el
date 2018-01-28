;;; keybindings.el --- define a function for keybinding

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layers elisp keybindings

;;; Commentary:

;;

;;; Code:

;; Surround
;; the same as the vim surround
;; cs <from> <to>        change surround
;; ds <aim>              delete surround
;; <visual> s <aim>      create surround

;; Hungry Delete Backwards
(define-key evil-hybrid-state-map (kbd "<C-backspace>") 'mp-hacking/hungry-delete)

;; Jump out from pairs
(memacs/define-evil-keybinding
 (list
  evil-hybrid-state-map
  evil-normal-state-map)
 "C-'" 'ar-leave-delimited-forward)

;; Scroll
;; Enable mouse support
(unless window-system
  (global-set-key (kbd "<mouse-4>") 'evil-previous-line)
  (global-set-key (kbd "<mouse-5>") 'evil-next-line))
(setq smooth-scroll-margin 10)

;; insert FF
(memacs/define-insert-keybinding "f" 'mp-hacking/insert-form-feed)

;; Buffer
(memacs/define-evil-normal-keybinding
 "C-n" 'next-buffer
 "C-p" 'previous-buffer)

;; Outshine
(spacemacs/set-leader-keys-for-minor-mode outline-minor-mode
  "M-RET"     'outshine-insert-heading
  "<backtab>" 'outshine-cycle-buffer)
(spacemacs/declare-prefix "o" "outshine")
(spacemacs/set-leader-keys
  ;; Insert
  "oi" 'outshine-insert-heading
  "ob" 'outshine-cycle-buffer

  ;; Narrowing
  "on" 'outshine-narrow-to-subtree
  "ow" 'widen

  ;; Structural edits and moves
  "oj" 'outline-forward-same-level
  "ok" 'outline-backward-same-level
  "oh" 'outline-up-heading
  "ol" 'outline-next-visible-heading
  "ou" 'outline-previous-visible-heading
  "oJ" 'outline-move-subtree-down
  "oK" 'outline-move-subtree-up
  "oH" 'outline-promote
  "oL" 'outline-demote
  )

;; Outline Ivy
(spacemacs/set-leader-keys "jo" 'oi-jump)

;;; keybindings.el ends here
