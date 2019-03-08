;;; legacy.el --- abandoned configuration

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: legacy

;; License:

;; MIT

;;; Commentary:

;;

;;; Code:


;; buffer swap
;; ace window is better: SPC w M
;; (spacemacs/declare-prefix "bS" "Swap")
(spacemacs/set-leader-keys
  "bS1" 'swap-buffer-window-no-follow-1  ;; swap buffer with window1
  "bS2" 'swap-buffer-window-no-follow-2  ;; swap buffer with window2
  "bS3" 'swap-buffer-window-no-follow-3  ;; swap buffer with window3
  "bS4" 'swap-buffer-window-no-follow-4  ;; swap buffer with window4
  "bS5" 'swap-buffer-window-no-follow-5  ;; swap buffer with window5
  "bS6" 'swap-buffer-window-no-follow-6  ;; swap buffer with window6
  "bS7" 'swap-buffer-window-no-follow-7  ;; swap buffer with window7
  "bS8" 'swap-buffer-window-no-follow-8  ;; swap buffer with window8
  "bS9" 'swap-buffer-window-no-follow-9) ;; swap buffer with window9


;; relative line number

(defun mp-ui/post-init-linum-relative ()
  "Change line format if not in gui"
  (with-eval-after-load 'linum-relative
    (unless (display-graphic-p)
      (setq linum-relative-format "%3s "))
    )
  )


;; Evil Play Mode And mp-hacking/init-string-inflection
(setq self-evil-play-operator-alist
      '((evil-change . c) (evil-delete . d)))

(define-minor-mode self-evil-play-mode
  "Buffer-local minor mode to define self evil keyboard."
  :keymap (make-sparse-keymap))

(define-globalized-minor-mode global-self-evil-play-mode
  self-evil-play-mode
  (lambda ()
    (self-evil-play-mode 1))
  "Global minor mode to define self evil keyboard.")

(global-self-evil-play-mode)

(defun self-evil-play-interactive-setup ()
  (setq evil-inhibit-operator t)
  (list (assoc-default evil-this-operator self-evil-play-operator-alist)))

(defmacro self-evil-play-define-key (key-sequence func)
  "Binding evil operator with the func."
  (let ((func-name (intern (format "self-evil-play/%s"
                                   (nth 1 func)))))
    `(progn
       (evil-define-command ,func-name
         (operation)
         ,(format "Define %s for evil operator!"
                  (nth 1 func))
         (interactive (self-evil-play-interactive-setup))
         (cond
          ((eq operation (quote ,(intern (substring key-sequence 0 1))))
           (call-interactively ,func))))
       (evil-define-key 'operator
         self-evil-play-mode-map
         ,(substring key-sequence 1)
         (quote ,func-name)))))

(defun mp-hacking/init-string-inflection ()
  "Bind keys for string inflection,Want to turn fooBar into foo_bar? Press crs
(coerce to snake_case). MixedCase (sim), camelCase (sic), snake_case (sis), lisp_case (sil)
and UPPER_CASE (siu) are all just 3 keystrokes away."
  ;;TODO: let lisp case could back to snake_case or others.
  (use-package string-inflection
    :defer t
    :init
    (self-evil-play-define-key "crs" 'string-inflection-underscore)
    (self-evil-play-define-key "crm" 'string-inflection-camelcase)
    (self-evil-play-define-key "crc" 'string-inflection-lower-camelcase)
    (self-evil-play-define-key "cru" 'string-inflection-upcase)
    (self-evil-play-define-key "crl" 'string-inflection-lisp)
    )
  )


;;;; Auto Org Agenda

(defun mp-org/org-agenda-reload-files ()
  "Reset the default value of org-agenda-reload-files."
  (interactive)
  (setq-default org-agenda-files (find-lisp-find-files org-directory "\.org$"))
  (message "Reload org files success!")
  )

(defun mp-org/auto-org-agenda-task ()
  "If auto org agenda task is not close.
Switch to @org -> reload org agenda file -> show agenda list"
  (unless close-auto-org-agenda-task
    (spacemacs/custom-perspective-@Org)
    (mp-org/org-agenda-reload-files)
    (org-agenda-list))
  )

(defun mp-org/switch-auto-org-agenda-task ()
  (interactive)
  (setq close-auto-org-agenda-task (not close-auto-org-agenda-task))
  (if close-auto-org-agenda-task
      (message "Closed auto org agenda task.")
    (message "Opened auto org agenda task."))
  )

(spacemacs/set-leader-keys

  ;; Switch auto org agenda task
  "aoT" 'mp-org/switch-auto-org-agenda-task

  ;; Org Agenda Reload
  "aor" 'mp-org/org-agenda-reload-files
  )

(setq close-auto-org-agenda-task t)


;;;; Autoescape

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
(spacemacs/defer-until-after-user-config
 (lambda ()
   (add-hook 'focus-in-hook 'memacs/autoescape-use-english-layout)
   (add-hook 'focus-out-hook 'memacs/autoescape-recover-outside-layout)
   (add-hook 'kill-emacs-hook 'memacs/autoescape-recover-outside-layout)))

;;; legacy.el ends here
