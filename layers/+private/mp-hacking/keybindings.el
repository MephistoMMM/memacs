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
(spacemacs/set-leader-keys "if" 'mp-hacking/insert-form-feed)

;; Buffer
(memacs/define-evil-normal-keybinding
 "C-n" 'next-buffer
 "C-p" 'previous-buffer)

;;; keybindings.el ends here
