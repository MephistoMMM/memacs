;;; keybindings.el --- define a function for keybinding

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layers elisp keybindings

;;; Commentary:

;;

;;; Code:


;;;; Base

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

;; Insert FF
(memacs/define-insert-keybinding "f" 'mp-hacking/insert-form-feed)

;; Buffer
;; (memacs/define-evil-normal-keybinding
;;  "C-n" 'next-buffer
;;  "C-p" 'previous-buffer)
;; keymap used in the popup menu
(setq switch-keymap (make-sparse-keymap))
(define-key switch-keymap (kbd "<tab>") 'popup-next)
(define-key switch-keymap (kbd "C-n") 'popup-next)
(define-key switch-keymap (kbd "j") 'popup-next)
(define-key switch-keymap (kbd "C-p") 'popup-previous)
(define-key switch-keymap (kbd "k") 'popup-previous)
(define-key switch-keymap (kbd "C-j") 'popup-select)
(define-key switch-keymap (kbd "<return>") 'popup-select)
(define-key evil-normal-state-map (kbd "C-p") 'mp-hacking/buffer-switch)
(define-key evil-normal-state-map (kbd "C-n") 'mp-hacking/buffer-switch)

;; terminal here
(spacemacs/set-leader-keys
  "jt" 'mp-hacking/terminal-here)


;;;; Outshine
(spacemacs/set-leader-keys-for-minor-mode outline-minor-mode
  "<backtab>" 'outshine-cycle-buffer)
(memacs/define-insert-keybinding "h" 'outshine-insert-heading)
(spacemacs/declare-prefix "o" "outshine")
(spacemacs/set-leader-keys
  ;; Insert
  "oi" 'outshine-insert-heading
  "ob" 'outshine-cycle-buffer

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

;; Narrowing
(spacemacs/set-leader-keys "no" 'outshine-narrow-to-subtree)


;;;; Outline Ivy
(spacemacs/set-leader-keys "jo" 'oi-jump)


;;;; Better Default
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "M-h") 'backward-kill-word)

;;; keybindings.el ends here
