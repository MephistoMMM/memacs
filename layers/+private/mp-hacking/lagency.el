;;; lagency.el --- abandoned configuration

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: lagency

;; License:

;; MIT

;;; Commentary:

;;

;;; Code:

;; buffer swap
;; ace window is better: SPC w M
;; (spacemacs/declare-prefix "bS" "Swap")
;; (spacemacs/set-leader-keys "bS1" 'swap-buffer-window-no-follow-1) ; swap buffer with window1
;; (spacemacs/set-leader-keys "bS2" 'swap-buffer-window-no-follow-2) ; swap buffer with window2
;; (spacemacs/set-leader-keys "bS3" 'swap-buffer-window-no-follow-3) ; swap buffer with window3
;; (spacemacs/set-leader-keys "bS4" 'swap-buffer-window-no-follow-4) ; swap buffer with window4
;; (spacemacs/set-leader-keys "bS5" 'swap-buffer-window-no-follow-5) ; swap buffer with window5
;; (spacemacs/set-leader-keys "bS6" 'swap-buffer-window-no-follow-6) ; swap buffer with window6
;; (spacemacs/set-leader-keys "bS7" 'swap-buffer-window-no-follow-7) ; swap buffer with window7
;; (spacemacs/set-leader-keys "bS8" 'swap-buffer-window-no-follow-8) ; swap buffer with window8
;; (spacemacs/set-leader-keys "bS9" 'swap-buffer-window-no-follow-9) ; swap buffer with window9


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


;;; lagency.el ends here
