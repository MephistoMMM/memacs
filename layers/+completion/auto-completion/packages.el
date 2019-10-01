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
        company
        company-statistics
        counsel
        fuzzy
        hippie-exp
        (ivy-yasnippet :requires ivy)
        yasnippet
        yasnippet-snippets
        ))

(defun auto-completion/init-auto-yasnippet ()
  (use-package auto-yasnippet
    :defer t
    :init
    (progn
      (setq aya-persist-snippets-dir
            (or auto-completion-private-snippets-directory
               (concat spacemacs-start-directory "snippets/")))
      )))

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
      ;; start posframe
      (when (configuration-layer/package-used-p 'company-posframe)
        (company-posframe-mode 1))
      (spacemacs|diminish company-mode " ⓐ" " a")

      ;; key bindings
      (spacemacs//auto-completion-set-RET-key-behavior 'company)
      (spacemacs//auto-completion-set-TAB-key-behavior 'company)
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

(defun auto-completion/pre-init-counsel ()
    (spacemacs|use-package-add-hook company
      :post-config
      (define-key company-active-map (kbd "C-s") 'counsel-company)))

(defun auto-completion/init-fuzzy ()
  (use-package fuzzy :defer t))

(defun auto-completion/init-hippie-exp ()
  ;; replace dabbrev-expand
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
          try-complete-lisp-symbol
          ;; Yasnippet
          ;; Try to expand yasnippet snippets based on prefix
          yas-hippie-try-expand)))

(defun auto-completion/init-ivy-yasnippet ()
  (use-package ivy-yasnippet
    :defer t
    :init
    (progn
      (setq ivy-yasnippet-expand-keys nil)
      )))

;; DOCUMENTS: http://joaotavora.github.io/yasnippet/snippet-development.html
(defun auto-completion/init-yasnippet ()
  (use-package yasnippet
    :commands (yas-global-mode yas-minor-mode yas-activate-extra-mode)
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
      (spacemacs|add-toggle yasnippet
        :mode yas-minor-mode
        :documentation "Enable snippets."
        :evil-leader "ty")
      (spacemacs/add-to-hooks
       'spacemacs/force-yasnippet-off '(term-mode-hook
                                        shell-mode-hook
                                        eshell-mode-hook))
      (spacemacs|require 'yasnippet)
      (spacemacs/add-to-hooks 'spacemacs/load-yasnippet '(prog-mode-hook
                                                          markdown-mode-hook
                                                        org-mode-hook)))
    :config
    (progn
      (spacemacs|diminish yas-minor-mode " ⓨ" " y")
      )))

(defun auto-completion/init-yasnippet-snippets ())
