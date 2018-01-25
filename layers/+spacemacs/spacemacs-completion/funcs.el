;;; funcs.el --- Spacemacs Completion Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



;; Ivy

(defun spacemacs//ivy-hjkl-navigation (style)
  "Set navigation on 'hjkl' for the given editing STYLE."
  (cond
   ((or (eq 'vim style)
        (and (eq 'hybrid style)
             hybrid-mode-enable-hjkl-bindings))
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
    (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
    ;; Move C-h to C-S-h
    (define-key ivy-minibuffer-map (kbd "C-S-h") help-map)
    (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "<escape>")
      'minibuffer-keyboard-quit))
   (t
    (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-alt-done)
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
    (define-key ivy-minibuffer-map (kbd "C-h") nil)
    (define-key ivy-minibuffer-map (kbd "C-l") nil))))

(defun spacemacs//ivy-matcher-desc ()
  (replace-regexp-in-string "ivy--" "" (format "%s" ivy--regex-function)))


;; Ido

(defun spacemacs//ido-minibuffer-setup ()
  "Setup the minibuffer."
  ;; Since ido is implemented in a while loop where each
  ;; iteration setup a whole new minibuffer, we have to keep
  ;; track of any activated ido navigation transient-state and force
  ;; the reactivation at each iteration.
  (when spacemacs--ido-navigation-ts-enabled
    (spacemacs/ido-navigation-transient-state/body)))

(defun spacemacs//ido-setup ()
  (when spacemacs--ido-navigation-ts-face-cookie-minibuffer
    (face-remap-remove-relative
     spacemacs--ido-navigation-ts-face-cookie-minibuffer))
  ;; be sure to wipe any previous transient-state flag
  (setq spacemacs--ido-navigation-ts-enabled nil)
  ;; overwrite the key bindings for ido vertical mode only
  (define-key ido-completion-map (kbd "C-<return>") 'ido-select-text)
  ;; use M-RET in terminal
  (define-key ido-completion-map "\M-\r" 'ido-select-text)
  (define-key ido-completion-map (kbd "C-h") 'ido-delete-backward-updir)
  (define-key ido-completion-map (kbd "C-j") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-k") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-l") 'ido-exit-minibuffer)
  (define-key ido-completion-map (kbd "C-n") 'ido-next-match)
  (define-key ido-completion-map (kbd "C-p") 'ido-prev-match)
  (define-key ido-completion-map (kbd "C-S-h") 'ido-prev-match-dir)
  (define-key ido-completion-map (kbd "C-S-j") 'next-history-element)
  (define-key ido-completion-map (kbd "C-S-k") 'previous-history-element)
  (define-key ido-completion-map (kbd "C-S-l") 'ido-next-match-dir)
  (define-key ido-completion-map (kbd "C-S-n") 'next-history-element)
  (define-key ido-completion-map (kbd "C-S-p") 'previous-history-element)
  ;; ido-other window maps
  (define-key ido-completion-map (kbd "C-o") 'spacemacs/ido-invoke-in-other-window)
  (define-key ido-completion-map (kbd "C-s") 'spacemacs/ido-invoke-in-vertical-split)
  (define-key ido-completion-map (kbd "C-t") 'spacemacs/ido-invoke-in-new-frame)
  (define-key ido-completion-map (kbd "C-v") 'spacemacs/ido-invoke-in-horizontal-split)
  ;; initiate transient-state
  (define-key ido-completion-map (kbd "M-SPC") 'spacemacs/ido-navigation-transient-state/body)
  (define-key ido-completion-map (kbd "S-M-SPC") 'spacemacs/ido-navigation-transient-state/body))

(defun spacemacs/ido-invoke-in-other-window ()
  "signals ido mode to switch to (or create) another window after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'other)
  (ido-exit-minibuffer))

(defun spacemacs/ido-invoke-in-horizontal-split ()
  "signals ido mode to split horizontally and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'horizontal)
  (ido-exit-minibuffer))

(defun spacemacs/ido-invoke-in-vertical-split ()
  "signals ido mode to split vertically and switch after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'vertical)
  (ido-exit-minibuffer))

(defun spacemacs/ido-invoke-in-new-frame ()
  "signals ido mode to create a new frame after exiting"
  (interactive)
  (setq ido-exit-minibuffer-target-window 'frame)
  (ido-exit-minibuffer))

(defun spacemacs//ido-navigation-ts-set-face ()
  "Set faces for ido navigation transient-state."
  (setq spacemacs--ido-navigation-ts-face-cookie-minibuffer
        (face-remap-add-relative
         'minibuffer-prompt
         'spacemacs-ido-navigation-ts-face)))

(defun spacemacs//ido-navigation-ts-on-enter ()
  "Initialization of ido transient-state."
  (setq spacemacs--ido-navigation-ts-enabled t)
  (spacemacs//ido-navigation-ts-set-face))

(defun spacemacs//ido-navigation-ts-on-exit ()
  "Action to perform when exiting ido transient-state."
  (face-remap-remove-relative
   spacemacs--ido-navigation-ts-face-cookie-minibuffer))

(defun spacemacs//ido-navigation-ts-full-doc ()
  "Full documentation for ido navigation transient-state."
  "
 [?]          display this help
 [e]          enter dired
 [j] [k]      next/previous match
 [J] [K]      sub/parent directory
 [h]          delete backward or parent directory
 [l]          select match
 [n] [p]      next/previous directory in history
 [o]          open in other window
 [s]          open in a new horizontal split
 [t]          open in other frame
 [v]          open in a new vertical split
 [q]          quit")
