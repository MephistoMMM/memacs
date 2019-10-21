;;; packages.el --- Java functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Lukasz Klich <klich.lukasz@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun spacemacs//java-backend ()
  "Returns selected backend."
  (if java-backend
      java-backend
    (when (configuration-layer/layer-used-p 'lsp) 'lsp)))

(defun spacemacs//java-setup-backend ()
  "Conditionally setup java backend."
  (spacemacs//java-setup-lsp))

(defun spacemacs//java-setup-company ()
  "Conditionally setup company based on backend."
  (spacemacs//java-setup-lsp-company))

(defun spacemacs//java-setup-dap ()
  "Conditionally setup elixir DAP integration."
  ;; currently DAP is only available using LSP
  (pcase (spacemacs//java-backend)
    (`lsp (spacemacs//java-setup-lsp-dap))))

(defun spacemacs//java-setup-flycheck ()
  "Conditionally setup flycheck based on backend."
  (spacemacs//java-setup-lsp-flycheck))


;; Maven

(defun spacemacs/mvn-clean-compile ()
  "Recompile using maven."
  (interactive)
  (mvn-clean)
  (mvn-compile))


;; Misc

(defun spacemacs//java-delete-horizontal-space ()
  (when (s-matches? (rx (+ (not space)))
                    (buffer-substring (line-beginning-position) (point)))
    (delete-horizontal-space t)))


;; LSP Java

(defun spacemacs//java-setup-lsp ()
  "Setup LSP Java."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (require 'lsp-java)
        (lsp))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//java-setup-lsp-company ()
  "Setup lsp auto-completion."
  (if (configuration-layer/layer-used-p 'lsp)
      (progn
        (spacemacs|add-company-backends
          :backends company-lsp
          :modes java-mode
          :append-hooks nil
          :call-hooks t)
        (company-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))

(defun spacemacs//java-setup-lsp-dap ()
  "Setup DAP integration."
  (require 'dap-java))

(defun spacemacs//java-setup-lsp-flycheck ()
  "Setup LSP Java syntax checking."
  (if (configuration-layer/layer-used-p 'lsp)
      (when (spacemacs/enable-flycheck 'java-mode)
        (require 'lsp-ui-flycheck)
        (lsp-ui-flycheck-enable nil)
        (flycheck-mode))
    (message "`lsp' layer is not installed, please add `lsp' layer to your dotfile.")))
