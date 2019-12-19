;;; funcs.el --- Auto-completion functions File -*- lexical-binding: t; -*-
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



(spacemacs|add-toggle auto-completion
  :status (bound-and-true-p company-mode)
  :on
  (progn
    (company-mode)
    (message "Enabled auto-completion (using %S)."
             auto-completion-front-end))
  :off
  (progn (company-mode -1)
         (message "Disabled auto-completion."))
  :documentation "Enable auto-completion."
  :evil-leader "ta")


;; company backends declaration macro

(defmacro spacemacs|add-company-backends (&rest props)
  "Add and enable company backends.
This function should be called exclusively in `post-init-company' functions or
`init-company-xxx' function where xxx is company backend package.

Available PROPS:

`:backends BACKENDS'
   One or several symbols or lists representing a company backend or a list of
   company backends.

`:modes MODES'
    One or several modes where BACKENDS will be added.

`:variables VAR VALUE'
    One or several VAR VALUE pairs (similar to layer variables).
    These variables are made buffer local so their values are set only for
    the given MODES.

`:from SYMBOL'
    Advanced property aimed at avoiding hook function name conflicts when
    `:variables' property is used in several calls to this macro for the same
    MODES.

`:append-hook BOOLEAN'
    Advanced property to control whether hooks functions are hooked or not,
    if non-nil hook functions are appended to modes hooks passed as `:modes'.

`:call-hooks BOOLEAN'
    if non-nil then hooked functions are called right away."
  (declare (indent 0))
  (let* ((backends (spacemacs/mplist-get-values props :backends))
         (modes (spacemacs/mplist-get-values props :modes))
         (variables (spacemacs/mplist-get-values props :variables))
         (from (spacemacs/mplist-get-value props :from))
         (hooks (if (memq :append-hooks props)
                    (spacemacs/mplist-get-value props :append-hooks)
                  t))
         (call-hooks (when (memq :call-hooks props)
                       (spacemacs/mplist-get-value props :call-hooks)))
         (result '(progn)))
    (dolist (mode modes)
      (let ((backends-var-name (intern (format "company-backends-%S" mode)))
            (raw-backends-var-name (intern (format "company-backends-%S-raw"
                                                   mode)))
            (init-func-name (intern (format "spacemacs//init-company-%S" mode)))
            (vars-func-name (intern
                             (format "spacemacs//init-company-vars-%S%s" mode
                                     (if from (format "-%S" from) ""))))
            (mode-hook-name (intern (format "%S-hook" mode))))
        ;; declare buffer local company-backends variable
        (push `(defvar ,raw-backends-var-name
                 spacemacs-default-company-backends
                 ,(format "Company backend list for %S." mode)) result)
        (push `(defvar ,backends-var-name ,raw-backends-var-name
                 ,(format "Company backend list for %S." mode)) result)
        ;; add backends
        (dolist (backend backends)
          (push `(add-to-list ',raw-backends-var-name ',backend) result))
        ;; define initialization hook function
        (push `(defun ,init-func-name ()
                 ,(format "Initialize company for %S." mode)
                 (if auto-completion-enable-snippets-in-popup
                     (setq ,backends-var-name
                           (mapcar 'spacemacs//show-snippets-in-company
                                   ,raw-backends-var-name))
                   (setq ,backends-var-name ,raw-backends-var-name))
                 (set (make-variable-buffer-local 'auto-completion-front-end)
                      'company)
                 (set (make-variable-buffer-local 'company-backends)
                      ,backends-var-name)) result)
        (when call-hooks
          (push `(,init-func-name) result))
        (when hooks
          (push `(add-hook ',mode-hook-name ',init-func-name t) result))
        ;; define variables hook function
        (when variables
          (let ((variables-copy variables)
                (vars-func `(defun ,vars-func-name ()
                              ,(format "Define company local variables for %S."
                                       mode)))
                vars)
            (while variables-copy
              (let* ((var (pop variables-copy))
                     (forms
                      (when (consp variables-copy)
                        `(set (make-variable-buffer-local ',var)
                              ,(eval (pop variables-copy))))))
                (when forms (push forms vars))))
            (push (append vars-func vars) result))
          (when call-hooks
            (push `(,vars-func-name) result))
          (when hooks
            (push `(add-hook ',mode-hook-name ',vars-func-name t) result)))
        (when hooks
          (push `(add-hook ',mode-hook-name 'company-mode t) result))))
    ;; return the expanded macro in correct order
    (reverse result)))

(defmacro spacemacs|disable-company (mode)
  "Disable company for the given MODE.
MODE parameter must match the :modes values used in the call to
`spacemacs|add-company-backends'."
  (let ((mode-hook-name (intern (format "%S-hook" mode)))
        (func (intern (format "spacemacs//init-company-%S" mode))))
    `(progn
       (remove-hook ',mode-hook-name ',func)
       (remove-hook ',mode-hook-name 'company-mode))))

(defun spacemacs//show-snippets-in-company (backend)
  (if (or (not auto-completion-enable-snippets-in-popup)
          (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))


;; auto-completion key bindings functions
(defun spacemacs//company-complete-common-or-cycle-backward ()
  "Complete common prefix or cycle backward."
  (interactive)
  (company-complete-common-or-cycle -1))

(defun spacemacs//auto-completion-set-RET-key-behavior (package)
  "Bind RET key appropriately for the given PACKAGE and value of
`auto-completion-return-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-return-key-behavior)
        (define-key map [return] 'company-complete-selection)
        (define-key map (kbd "RET") 'company-complete-selection)
        (define-key map (kbd "<C-return>") 'memacs/company-complete-selection))
       (t
        (define-key map [return] 'nil)
        (define-key map (kbd "RET") 'nil)))))
   (t (message "Not yet implemented for package %S" package))))

(defun spacemacs//auto-completion-set-TAB-key-behavior (package)
  "Bind TAB key appropriately for the given PACKAGE and value of
`auto-completion-tab-key-behavior'."
  (cond
   ((eq 'company package)
    (let ((map company-active-map))
      (cond
       ((eq 'complete auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-selection)
        (define-key map (kbd "<tab>") 'company-complete-selection))
       ((eq 'cycle auto-completion-tab-key-behavior)
        (define-key map (kbd "TAB") 'company-complete-common-or-cycle)
        (define-key map (kbd "<tab>") 'company-complete-common-or-cycle)
        (define-key map (kbd "<S-tab>")
          'spacemacs//company-complete-common-or-cycle-backward)
        (define-key map (kbd "<backtab>")
          'spacemacs//company-complete-common-or-cycle-backward))
       (t
        (define-key map (kbd "TAB") nil)
        (define-key map (kbd "<tab>") nil)))))
   (t (message "Not yet implemented for package %S" package))))


;; Transformers

(defun spacemacs//company-transformer-cancel (candidates)
  "Cancel completion if prefix is in the list
`company-mode-completion-cancel-keywords'"
  (unless (member company-prefix company-mode-completion-cancel-keywords)
    candidates))



(defvar-local company-fci-mode-on-p nil)

(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))

(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))


;; ivy-yas

(defun spacemacs/ivy-yas ()
  "Lazy load ivy-yasnippet"
  (interactive)
  (spacemacs/load-yasnippet)
  (require 'ivy-yasnippet)
  (call-interactively 'ivy-yasnippet))


;; Yasnippet

(defun spacemacs/load-yasnippet ()
  (unless yas-global-mode (yas-global-mode 1))
  (yas-minor-mode 1))

(defun spacemacs/force-yasnippet-off ()
  (yas-minor-mode -1)
  (setq yas-dont-activate t))

(defun memacs/describe-yasnippets ()
  "Show yasnippets' descriptions in a read-only window.

Press 'q' to quit."
  (interactive)
  (select-window (yas-describe-tables)))

;; A solution for completion by TAB in yas expanding state
;; inspired from https://www.emacswiki.org/emacs/CompanyMode#toc10
(defun memacs//check-expansion ()
  (save-excursion
    (if (looking-at "\\_>") t
      (backward-char 1)
      (if (looking-at "\\.") t
        (backward-char 1)
        (if (looking-at "->") t nil)))))

(defun memacs/tab-complete-or-next-field ()
  (interactive)
  (when (company-mode)
    (if company-candidates
        ;; yes
        (company-complete-selection)
      ;; no
      (if (not (memacs//check-expansion))
          ;; yes
          (yas-next-field-or-maybe-expand)
        ;; no
        (if (not (company-manual-begin))
            ;; yes
            (yas-next-field-or-maybe-expand)
          ;; no
          (if (or (null company-candidates)
                 (and lsp-mode (<= (length company-candidates) 1)))
              (progn
                (company-abort)
                (yas-next-field-or-maybe-expand)))
          )))
    ))


;; Auto-Yasnippet

(defun spacemacs/auto-yasnippet-expand ()
  "Call `yas-expand' and switch to `insert state'"
  (interactive)
  (call-interactively 'aya-expand)
  (evil-insert-state))


;; complete selection function
(defun memacs/company-complete-selection ()
  "Insert the selected candidate."
  (interactive)
  (when (company-manual-begin)
    (let ((result (nth company-selection company-candidates)))
      (company--insert-candidate result)
      (company-abort)
      ))
  )

;; quit to normal state
(defun memacs/company-escape ()
  "Quit company toolit to evil normal state"
  (interactive)
  (company-abort)
  (evil-normal-state))


;; hippie-expand

;; A solution for indent always expand while pressing TAB.
;; inspired from https://www.emacswiki.org/emacs/CompanyMode#toc10
(defun memacs//check-indent ()
  (if (looking-back "^\\\s*") t))

(defun memacs/tab-indent-or-hippie-expand (arg)
  (interactive "P")
  (if (memacs//check-indent)
      ;; yes
      (insert-tab)
    ;; no
    (unless (call-interactively 'hippie-expand arg)
      (insert-tab))))
