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
(define-key evil-normal-state-map (kbd "M-<return>") 'mp-hacking/buffer-switch)

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

;;;; Goenv
(with-eval-after-load 'go-mode
  (spacemacs/set-leader-keys-for-major-mode 'go-mode "Va" 'goenv-activate)
  (spacemacs/set-leader-keys-for-major-mode 'go-mode "Vd" 'goenv-deactivate))


;;;; Better Default

(when (spacemacs/system-is-mac)
  (global-set-key [(meta a)] 'mark-whole-buffer)
  (global-set-key [(meta v)] 'yank)
  (global-set-key [(meta c)] 'kill-ring-save)
  (global-set-key [(meta s)] 'save-buffer)
  (global-set-key [(meta w)]
                  (lambda () (interactive) (delete-window)))
  (global-set-key [(meta z)] 'undo)
  ;; (global-set-key [(meta l)] 'hippie-expand)
  ;; C-v in ivy minibuffer
  (define-key ivy-minibuffer-map (kbd "M-v") 'yank)
  ;; show ivy recent contents is C-r
  )

;; TODO: create a mp-osx layer and mv these better default codes to mp-osx
(defvar memacs-autoescape-english-layout-name "ABC"
  "English layout name in your macOS system")

(setq memacs-autoescape--origin-outside-layout-name "ABC")

(defun memacs/autoescape-use-english-layout()
  "Change input source to english layout while emacs frame focused."
  (unless (evil-hybrid-state-p)
    (setq memacs-autoescape--origin-outside-layout-name (shell-command-to-string "textinputsource"))
    (unless (string= memacs-autoescape--origin-outside-layout-name
                     memacs-autoescape-english-layout-name)
      (start-process-shell-command "changeInputSource" nil
                                   (concat "textinputsource -s "
                                           memacs-autoescape-english-layout-name))
      ))
  )

(defun memacs/autoescape-recover-outside-layout()
  "Recover input source to origin layout while emacs frame unfocused."
  (unless (string=
           (shell-command-to-string "textinputsource")
           memacs-autoescape--origin-outside-layout-name)
    (call-process-shell-command (concat "textinputsource -s "
                                         memacs-autoescape--origin-outside-layout-name)))
  )

;; Following statements sometimes doesn't run , so let it be called after user-config
(spacemacs/defer-until-after-user-config (lambda ()
                                           (add-hook 'focus-in-hook 'memacs/autoescape-use-english-layout)
                                           (add-hook 'focus-out-hook 'memacs/autoescape-recover-outside-layout)
                                           (add-hook 'kill-emacs-hook 'memacs/autoescape-recover-outside-layout)))

;;; keybindings.el ends here
