;;; packages.el --- Auto-completion Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq auto-completion-packages
      '(
        auto-yasnippet
        auto-complete
        ac-ispell
        company
        (company-quickhelp :toggle auto-completion-enable-help-tooltip)
        company-statistics
        fuzzy
        hippie-exp
        yasnippet
        yasnippet-snippets
        ))

;; TODO replace by company-ispell which comes with company
;; to be moved to spell-checking layer as well
(defun auto-completion/init-ac-ispell ()
  (use-package ac-ispell
    :defer t
    :init
    (progn
      (setq ac-ispell-requires 4)
      (with-eval-after-load 'auto-complete
        (ac-ispell-setup))
      ;; (add-hook 'markdown-mode-hook 'ac-ispell-ac-setup)
      )))

(defun auto-completion/init-auto-complete ()
  (use-package auto-complete
    :defer t
    :init
    (setq ac-auto-start 0
          ac-delay auto-completion-idle-delay
          ac-quick-help-delay 1.
          ac-use-fuzzy t
          ac-fuzzy-enable t
          ac-comphist-file (concat spacemacs-cache-directory "ac-comphist.dat")
          ;; use 'complete when auto-complete is disabled
          tab-always-indent 'complete
          ac-dwim t)
    :config
    (progn
      (require 'auto-complete-config)
      (setq-default ac-sources '(ac-source-abbrev
                                 ac-source-dictionary
                                 ac-source-words-in-same-mode-buffers))
      (when (configuration-layer/package-used-p 'yasnippet)
        (add-to-list 'ac-sources 'ac-source-yasnippet))
      (add-to-list 'completion-styles 'initials t)
      (define-key ac-completing-map (kbd "C-j") 'ac-next)
      (define-key ac-completing-map (kbd "C-k") 'ac-previous)
      (define-key ac-completing-map (kbd "<S-tab>") 'ac-previous)
      (spacemacs|diminish auto-complete-mode " ⓐ" " a"))))

(defun auto-completion/init-auto-yasnippet ()
  (use-package auto-yasnippet
    :defer t
    :init
    (progn
      (setq aya-persist-snippets-dir
            (or auto-completion-private-snippets-directory
                (concat spacemacs-start-directory "snippets/")))
      (spacemacs/set-leader-keys
        "sc" 'aya-create
        "se" 'spacemacs/auto-yasnippet-expand
        "sp" 'aya-persist-snippet))))

(defun auto-completion/init-company ()
  (use-package company
    :defer t
    :init
    (progn
      (setq company-idle-delay auto-completion-idle-delay
            company-minimum-prefix-length 2
            company-require-match nil
            company-dabbrev-ignore-case t
            company-dabbrev-downcase nil)

      (add-hook 'company-completion-started-hook 'company-turn-off-fci)
      (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
      (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci))
    :config
    (progn
      (spacemacs|diminish company-mode " ⓐ" " a")

      ;; key bindings
      (defun spacemacs//company-complete-common-or-cycle-backward ()
        "Complete common prefix or cycle backward."
        (interactive)
        (company-complete-common-or-cycle -1))
      (spacemacs//auto-completion-set-RET-key-behavior 'company)
      (spacemacs//auto-completion-set-TAB-key-behavior 'company)
      (spacemacs//auto-completion-setup-key-sequence 'company)

      (let ((map company-active-map))
        (define-key map (kbd "C-j") 'company-select-next)
        (define-key map (kbd "C-k") 'company-select-previous)
        (define-key map (kbd "C-l") 'memacs/company-complete-selection)
        (define-key map (kbd "C-s") 'company-filter-candidates)
        (define-key map (kbd "C-d") 'company-show-doc-buffer))
      (let ((map company-search-map))
        (define-key map (kbd "C-j") 'company-select-next)
        (define-key map (kbd "C-k") 'company-select-previous)
        (define-key map (kbd "C-l") 'memacs/company-complete-selection))
      (when (require 'company-quickhelp nil 'noerror)
        (evil-define-key 'insert company-quickhelp-mode-map (kbd "C-k") 'company-select-previous))

      (setq company-transformers '(spacemacs//company-transformer-cancel
                                   company-sort-by-occurrence))

      )))

(defun auto-completion/init-company-statistics ()
  (use-package company-statistics
    :if auto-completion-enable-sort-by-usage
    :defer t
    :init
    (progn
      (setq company-statistics-file (concat spacemacs-cache-directory
                                            "company-statistics-cache.el"))
      (add-hook 'company-mode-hook 'company-statistics-mode))))

(defun auto-completion/init-fuzzy ()
  (use-package fuzzy :defer t))

(defun auto-completion/init-company-quickhelp ()
  (use-package company-quickhelp
    :commands company-quickhelp-manual-begin
    :init
    (spacemacs|do-after-display-system-init
     (with-eval-after-load 'company
       (setq company-frontends (delq 'company-echo-metadata-frontend company-frontends))
       (define-key company-active-map (kbd "M-h") #'company-quickhelp-manual-begin)
       (unless (eq auto-completion-enable-help-tooltip 'manual)
         (company-quickhelp-mode))))))

(defun auto-completion/init-hippie-exp ()
  ;; replace dabbrev-expand
  (define-key evil-hybrid-state-map (kbd "C-h")   'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(
          ;; Try to expand word "dynamically", searching the current buffer.
          try-expand-dabbrev
          ;; Try to expand word "dynamically", searching all other buffers.
          try-expand-dabbrev-all-buffers
          ;; Try to expand word "dynamically", searching the kill ring.
          try-expand-dabbrev-from-kill
          ;; Try to complete text as a file name, as many characters as unique.
          try-complete-file-name-partially
          ;; Try to complete text as a file name.
          try-complete-file-name
          ;; Try to expand word before point according to all abbrev tables.
          try-expand-all-abbrevs
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-list
          ;; Try to complete the current line to an entire line in the buffer.
          try-expand-line
          ;; Try to complete as an Emacs Lisp symbol, as many characters as
          ;; unique.
          try-complete-lisp-symbol-partially
          ;; Try to complete word as an Emacs Lisp symbol.
          try-complete-lisp-symbol))
    ;; Try to expand yasnippet snippets based on prefix
    (add-to-list 'hippie-expand-try-functions-list 'yas-hippie-try-expand))

(defun auto-completion/init-yasnippet ()
  (use-package yasnippet
    :commands (yas-global-mode yas-minor-mode)
    :init
    (progn
      ;; We don't want undefined variable errors
      (defvar yas-global-mode nil)
      (setq yas-triggers-in-field t
            yas-wrap-around-region t)
      (setq yas-prompt-functions '(yas-completing-prompt))
      ;; disable yas minor mode map
      ;; use hippie-expand instead
      (setq yas-minor-mode-map (make-sparse-keymap))
      ;; configure snippet directories
      (let* ((spacemacs--auto-completion-dir
              (configuration-layer/get-layer-local-dir 'auto-completion))
             (emacs-directory-snippets-dir (concat
                                          spacemacs-start-directory
                                          "snippets/"))
             (spacemacs-layer-snippets-dir (expand-file-name
                                      "snippets"
                                      spacemacs--auto-completion-dir)))
        (setq yas-snippet-dirs nil)
        ;; ~/.emacs.d/layers/auto-completion/snippets
        (add-to-list 'yas-snippet-dirs spacemacs-layer-snippets-dir)
        ;; ~/.emacs.d/snippets
        (add-to-list 'yas-snippet-dirs emacs-directory-snippets-dir)
        ;; arbitrary directories in `auto-completion-private-snippets-directory'
        (when auto-completion-private-snippets-directory
          (if (listp auto-completion-private-snippets-directory)
              (setq yas-snippet-dirs (append yas-snippet-dirs auto-completion-private-snippets-directory))
            (add-to-list 'yas-snippet-dirs auto-completion-private-snippets-directory))))

      (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                          markdown-mode-hook
                                                          org-mode-hook))
      (spacemacs|add-toggle yasnippet
        :mode yas-minor-mode
        :documentation "Enable snippets."
        :evil-leader "ty")

      (spacemacs/add-to-hooks
       'spacemacs/force-yasnippet-off '(term-mode-hook
                                        shell-mode-hook
                                        eshell-mode-hook)))
    :config (spacemacs|diminish yas-minor-mode " ⓨ" " y")))

(defun auto-completion/init-yasnippet-snippets ())
