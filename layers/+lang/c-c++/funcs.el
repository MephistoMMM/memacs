;;; funcs.el --- C/C++ Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl-lib)
(require 'subr-x)

(defun memacs//c-mode-keybinding-modify ()
  (define-key c-mode-map (kbd ";") nil))

(defun spacemacs//c-c++-backend ()
  "Returns selected backend."
  (if c-c++-backend
      c-c++-backend
    (cond
     ((configuration-layer/layer-used-p 'lsp) 'lsp-clangd)
     (t nil))))

(defun spacemacs//c-c++-setup-backend ()
  "Conditionally setup c-c++ backend."
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (spacemacs//c-c++-setup-lsp-clangd))
    (`lsp-ccls (spacemacs//c-c++-setup-lsp-ccls))
    (`lsp-cquery (spacemacs//c-c++-setup-lsp-cquery))
    (`ycmd (spacemacs//c-c++-setup-ycmd))))

(defun spacemacs//c-c++-setup-company ()
  "Conditionally setup C/C++ company integration based on backend."
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (spacemacs//c-c++-setup-lsp-company))
    (`lsp-ccls (spacemacs//c-c++-setup-lsp-company))
    (`lsp-cquery (spacemacs//c-c++-setup-lsp-company))
    (`ycmd (spacemacs//c-c++-setup-ycmd-company))))

(defun spacemacs//c-c++-setup-dap ()
  "Conditionally setup C/C++ DAP integration based on backend."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (spacemacs//c-c++-setup-lsp-dap))
    (`lsp-ccls (spacemacs//c-c++-setup-lsp-dap))
    (`lsp-cquery (spacemacs//c-c++-setup-lsp-dap))))

(defun spacemacs//c-c++-setup-eldoc ()
  "Conditionally setup C/C++ eldoc integration based on backend."
  (pcase (spacemacs//c-c++-backend)
    ;; lsp setup eldoc on its own
    (`ycmd (spacemacs//c-c++-setup-ycmd-eldoc))))

(defun spacemacs//c-c++-setup-flycheck ()
  "Conditionally setup C/C++ flycheck integration based on backend."
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (spacemacs//c-c++-setup-lsp-flycheck))
    (`lsp-ccls (spacemacs//c-c++-setup-lsp-flycheck))
    (`lsp-cquery (spacemacs//c-c++-setup-lsp-flycheck))
    (`ycmd (spacemacs//c-c++-setup-ycmd-flycheck))))

(defun spacemacs//c-c++-setup-format ()
  "Conditionally setup format based on backend."
  (pcase (spacemacs//c-c++-backend)
    (`lsp-clangd (spacemacs//c-c++-setup-clang-format))
    (`lsp-ccls (spacemacs//c-c++-setup-clang-format))
    (`lsp-cquery (spacemacs//c-c++-setup-clang-format))))

(defun spacemacs//c-c++-setup-semantic ()
  "Conditionally setup semantic based on backend."
  (pcase (spacemacs//c-c++-backend)
    (`ycmd (spacemacs//c-c++-setup-ycmd-semantic))))


;; lsp

;; clang

(defun spacemacs//c-c++-setup-lsp-clangd ()
  "Setup LSP clangd."
  ;; extensions
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-clangd"
   'clangd-other-file "textDocument/switchSourceHeader" 'buffer-file-name)
  (set (make-local-variable 'lsp-disabled-clients) '(cquery ccls))
  (lsp))

;; ccls

(defun spacemacs//c-c++-setup-lsp-ccls ()
  "Setup LSP ccls."
  (require 'ccls)

  ;; semantic highlight
  (when c-c++-lsp-enable-semantic-highlight
    (setq ccls-sem-highlight-method c-c++-lsp-semantic-highlight-method)
    (when (eq 'rainbow c-c++-lsp-enable-semantic-highlight)
      (ccls-use-default-rainbow-sem-highlight)))

  ;; extensions
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'refs-address "textDocument/references" '(:role 128))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'refs-read "textDocument/references" '(:role 8))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'refs-write "textDocument/references" '(:role 16))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'callers "$ccls/call")
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'callees "$ccls/call" '(:callee t))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'base "$ccls/inheritance")

  ;; ccls features without a cquery analogue...
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'derived "$ccls/inheritance" '(:derived t))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'member-types "$ccls/member" `(:kind 2))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'member-functions "$ccls/member" `(:kind 3))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-ccls"
   'member-vars "$ccls/member" `(:kind 0))

  ;; key bindings
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
      ;; backend
      "bf" 'ccls-reload
      "bp" 'ccls-preprocess-file
      ;; goto
      "gf" 'find-file-at-point
      "gF" 'ffap-other-window
      ;; hierarchy
      "ghc" 'ccls-call-hierarchy
      "ghC" 'spacemacs/c-c++-lsp-ccls-call-hierarchy-inv
      "ghi" 'ccls-inheritance-hierarchy
      "ghI" 'spacemacs/c-c++-lsp-ccls-inheritance-hierarchy-inv
      ;; members
      "gmh" 'ccls-member-hierarchy)

    (spacemacs/lsp-bind-extensions-for-mode
     mode "c-c++" "lsp-ccls"
     "&" 'refs-address
     "R" 'refs-read
     "W" 'refs-write
     "c" 'callers
     "C" 'callees
     "v" 'vars
     "hb" 'base)

    (spacemacs/lsp-bind-extensions-for-mode
     mode "c-c++" "lsp-ccls"
     "hd" 'derived
     "mt" 'member-types
     "mf" 'member-functions
     "mv" 'member-vars))

  ;;(evil-set-initial-state 'ccls--tree-mode 'emacs)
  ;;evil-record-macro keybinding clobbers q in cquery-tree-mode-map for some reason?
  ;;(evil-make-overriding-map 'ccls-tree-mode-map)
  (set (make-local-variable 'lsp-disabled-clients) '(clangd cquery))
  (lsp))

;; cquery

(defun spacemacs//c-c++-setup-lsp-cquery ()
  "Setup LSP cquery."
  (require 'cquery)
  ;; configuration
  (setq cquery-cache-dir
        (if (null c-c++-lsp-cquery-cache-directory)
            (concat spacemacs-cache-directory "lsp-cquery")
          (concat (file-truename (file-name-as-directory
                                  c-c++-lsp-cquery-cache-directory)))))

  ;; semantic highlight
  (when c-c++-lsp-enable-semantic-highlight
    (setq cquery-sem-highlight-method c-c++-lsp-semantic-highlight-method)
    (when (eq 'rainbow c-c++-lsp-enable-semantic-highlight)
      (cquery-use-default-rainbow-sem-highlight)))

  ;; extensions
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-cquery"
   'refs-address "textDocument/references"
   '(plist-put (lsp--text-document-position-params) :context '(:role 128)))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-cquery"
   'refs-read "textDocument/references"
   '(plist-put (lsp--text-document-position-params) :context '(:role 8)))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-cquery"
   'refs-write "textDocument/references"
   '(plist-put (lsp--text-document-position-params) :context '(:role 16)))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-cquery"
   'callers "$cquery/callers")
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-cquery"
   'callees "$cquery/callers" '(:callee t))
  (spacemacs/lsp-define-extensions
   "c-c++" "lsp-cquery"
   'base "$cquery/base")

  ;; key bindings
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
      ;; backend
      "bf" 'cquery-freshen-index
      "bp" 'cquery-preprocess-file
      ;; goto
      "gf" 'find-file-at-point
      "gF" 'ffap-other-window
      ;; hierarchy
      "ghc" 'cquery-call-hierarchy
      "ghC" 'spacemacs/c-c++-lsp-cquery-call-hierarchy-inv
      "ghi" 'cquery-inheritance-hierarchy
      "ghI" 'spacemacs/c-c++-lsp-cquery-inheritance-hierarchy-inv
      ;; members
      "gmh" 'cquery-member-hierarchy)

    (spacemacs/lsp-bind-extensions-for-mode
     mode "c-c++" "lsp-cquery"
     "&" 'refs-address
     "R" 'refs-read
     "W" 'refs-write
     "c" 'callers
     "C" 'callees
     "v" 'vars
     "hb" 'base))

  ;;(evil-set-initial-state 'ccls--tree-mode 'emacs)
  ;;evil-record-macro keybinding clobbers q in cquery-tree-mode-map for some reason?
  ;;(evil-make-overriding-map 'ccls-tree-mode-map)
  (set (make-local-variable 'lsp-disabled-clients) '(ccls changd))
  (lsp))

(defun spacemacs//c-c++-setup-lsp-company ()
  "Setup lsp auto-completion."
  (spacemacs|add-company-backends
    :backends company-lsp
    :modes c-mode c++-mode
    :append-hooks nil
    :call-hooks t)
  (company-mode))

(defun spacemacs//c-c++-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-gdb-lldb))

(defun spacemacs//c-c++-setup-lsp-flycheck ()
  "Setup LSP syntax checking."
  (when (or (spacemacs/enable-flycheck 'c-mode)
            (spacemacs/enable-flycheck 'c++-mode))
    (require 'lsp-ui-flycheck)
    (lsp-ui-flycheck-enable nil)
    (flycheck-mode)))


;; gtags

(defun spacemacs/c-c++-tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (gtags-find-tag))

(defun spacemacs/c-c++-tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (gtags-find-rtag))

(defun spacemacs/c-c++-tags-find-symbol ()
  (interactive)
  (call-interactively 'gtags-find-symbol))

(defun spacemacs/c-c++-tags-find-references ()
  (interactive)
  (call-interactively 'gtags-find-rtag))

(defun spacemacs/c-c++-tags-find-file ()
  (interactive)
  (call-interactively 'gtags-find-file))

(defun spacemacs/c-c++-tags-imenu ()
  (interactive)
  (call-interactively 'idomenu))


;; ycmd

(defun spacemacs//c-c++-setup-ycmd ()
  "Setup ycmd backend."
  (add-to-list 'spacemacs-jump-handlers-c++-mode '(ycmd-goto :async t))
  (add-to-list 'spacemacs-jump-handlers-c-mode '(ycmd-goto :async t))
  (dolist (mode c-c++-modes)
    (spacemacs/set-leader-keys-for-major-mode mode
      "gG" 'ycmd-goto-imprecise))
  (ycmd-mode))

(defun spacemacs//c-c++-setup-ycmd-company ()
  "Setup ycmd auto-completion."
  (spacemacs|add-company-backends
    :backends company-ycmd
    :modes c-mode-common
    :append-hooks nil
    :call-hooks t))

(defun spacemacs//c-c++-setup-ycmd-eldoc ()
  "Setup ycmd eldoc integration."
  (ycmd-eldoc-setup))

(defun spacemacs//c-c++-setup-ycmd-flycheck ()
  "Setup ycmd syntax checking."
  (when (or (spacemacs/enable-flycheck 'c-mode)
            (spacemacs/enable-flycheck 'c++-mode))
    (flycheck-ycmd-setup)
    (flycheck-mode)))

(defun spacemacs//c-c++-setup-ycmd-semantic ()
  "Setup semantic for ycmd."
  (semantic-mode))


;; style

(defun spacemacs//c-toggle-auto-newline ()
  "Toggle auto-newline."
  (c-toggle-auto-newline 1))


;; clang

(defun spacemacs//c-c++-setup-clang-format ()
  "Setup clang format."
  (when c-c++-enable-clang-format-on-save
    (spacemacs/add-to-hooks 'spacemacs/clang-format-on-save c-c++-mode-hooks))
  (dolist (mode c-c++-modes)
    (spacemacs/declare-prefix-for-mode mode "=" "format")
    (spacemacs/set-leader-keys-for-major-mode mode
      "==" 'spacemacs/clang-format-region-or-buffer
      "=f" 'spacemacs/clang-format-function)))

(defun spacemacs/clang-format-function (&optional style)
  "Format the current function with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (c-mark-function)
    (clang-format (region-beginning) (region-end) style)
    (deactivate-mark) ; If the function is already formatted, then remove the mark
    (message "Formatted function %s" (c-defun-name))))

(defun spacemacs/clang-format-region-or-buffer (&optional style)
  "Format the current region or buffer with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (clang-format-region (region-beginning) (region-end) style)
          (message "Formatted region"))
      (progn
        (clang-format-buffer style)
        (message "Formatted buffer %s" (buffer-name))))))

(defun spacemacs//clang-format-on-save ()
  "Format the current buffer with clang-format on save when
`c-c++-enable-clang-format-on-save' is non-nil."
  (when c-c++-enable-clang-format-on-save
    (spacemacs/clang-format-region-or-buffer)))

(defun spacemacs/clang-format-on-save ()
  "Add before-save hook for clang-format."
  (add-hook 'before-save-hook 'spacemacs//clang-format-on-save nil t))


;; ccls

(defun spacemacs/c-c++-lsp-ccls-call-hierarchy-inv ()
  (interactive)
  (ccls-call-hierarchy t))

(defun spacemacs/c-c++-lsp-ccls-inheritance-hierarchy-inv ()
  (interactive)
  (ccls-inheritance-hierarchy t))


;; cquery

(defun spacemacs/c-c++-lsp-cquery-call-hierarchy-inv ()
  (interactive)
  (cquery-call-hierarchy t))

(defun spacemacs/c-c++-lsp-cquery-inheritance-hierarchy-inv ()
  (interactive)
  (cquery-inheritance-hierarchy t))


;; cpp-auto-include

(defalias 'spacemacs/c++-organize-includes 'cpp-auto-include)

(defun spacemacs//c++-organize-includes-on-save ()
  "Organize the includes on save when `c++-enable-organize-includes-on-save'
is non-nil."
  (when c++-enable-organize-includes-on-save
    (spacemacs/c++-organize-includes)))

(defun spacemacs/c++-organize-includes-on-save ()
  "Add before-save hook for c++-organize-includes."
  (add-hook 'before-save-hook
            #'spacemacs//c++-organize-includes-on-save nil t))


;; cquery
(defun memacs/cquery-caller-hierarchy ()
  "caller hierarchy"
  (interactive)
  (cquery-call-hierarchy nil))

(defun memacs/cquery-callee-hierarchy ()
  "callee hierarchy"
  (interactive)
  (cquery-call-hierarchy t))

(defun memacs/cquery-base-inheritance-hierarchy ()
  "base inheritance"
  (interactive)
  (cquery-inheritance-hierarchy nil))

(defun memacs/cquery-derived-inheritance-hierarchy ()
  "derived inheritance"
  (interactive)
  (cquery-inheritance-hierarchy t))
