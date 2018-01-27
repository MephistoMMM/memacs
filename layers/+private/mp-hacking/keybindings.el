;;; keybindings.el --- define a function for keybinding

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layers elisp keybindings

;;; Commentary:

;;

;;; Code:

(defun mp-hacking/hacking-keybinding-init ()
  "This fucntion provide more efficient keybindings for hacker.
   It should be called in 'dotspacemacs/user-config', sence it will cover the some
   default keybinding."
  ;; Surround
  ;; the same as the vim surround
  ;; cs <from> <to>        change surround
  ;; ds <aim>              delete surround
  ;; <visual> s <aim>      create surround

  ;; Expand
  (define-key evil-normal-state-map (kbd "e") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "e") 'er/expand-region)
  (define-key evil-normal-state-map (kbd "E") 'er/contract-region)
  (define-key evil-visual-state-map (kbd "E") 'er/contract-region)

  ;; Hungry Delete Backwards
  (define-key evil-hybrid-state-map (kbd "<C-backspace>") 'mp-hacking/hungry-delete)

  ;; Jump out from pairs
  (define-key evil-hybrid-state-map (kbd "C-'") 'ar-leave-delimited-forward)
  (define-key evil-normal-state-map (kbd "C-'") 'ar-leave-delimited-forward)

  ;; Scroll
  ;; Enable mouse support
  (unless window-system
    (global-set-key (kbd "<mouse-4>") 'evil-previous-line)
    (global-set-key (kbd "<mouse-5>") 'evil-next-line))
  (setq smooth-scroll-margin 10)

  ;; insert FF
  (spacemacs/set-leader-keys "if" 'mp-hacking/insert-form-feed)

  ;; Yasnippet
  ;; Use C-k to start complete snips and use TAB to go arround the points
  ;; How to write yasnippet: https://joaotavora.github.io/yasnippet/snippet-development.html or http://d.pr/n/1bHuv
  (spacemacs/declare-prefix "oy" "yasnippet")
  (spacemacs/set-leader-keys "oyn" 'yas-new-snippet)                      ;;owner yasnippet new
  (spacemacs/set-leader-keys "oyl" 'yas-load-snippet-buffer)              ;;owner yasnippet load
  (spacemacs/set-leader-keys "oyc" 'yas-load-snippet-buffer-and-close)    ;;owner yasnippet load and close
  (spacemacs/set-leader-keys "oyv" 'yas-visit-snippet-file)               ;;owner yasnippet visit file

  ;; Diff
  (spacemacs/declare-prefix "d" "diff")
  (spacemacs/set-leader-keys "dd" 'ediff)
  (spacemacs/set-leader-keys "dm" 'ediff3)
  )

;;; keybindings.el ends here
