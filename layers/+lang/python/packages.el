;;; packages.el --- Python Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq python-packages
      '(
        blacken
        company
        counsel-gtags
        eldoc
        evil-matchit
        flycheck
        ggtags
        importmagic
        live-py-mode
        (nose :location local)
        org
        pip-requirements
        pippel
        py-isort
        (pylookup :location local)
        pytest
        (python :location built-in)
        pyvenv
        stickyfunc-enhance
        xcscope
        yapfify
        ;; packages for Microsoft LSP backend
        (lsp-python-ms :requires lsp-mode)
        ))

(defun python/post-init-company ()
  ;; backend specific
  (add-hook 'python-mode-local-vars-hook #'spacemacs//python-setup-company)
  (spacemacs|add-company-backends
    :backends (company-files company-capf)
    :modes inferior-python-mode
    :variables
    company-minimum-prefix-length 0
    company-idle-delay 0.3)
  (when (configuration-layer/package-used-p 'pip-requirements)
    (spacemacs|add-company-backends
      :backends company-capf
      :modes pip-requirements-mode)))

(defun python/init-blacken ()
  (use-package blacken
    :defer t
    :init
    (progn
      (spacemacs//bind-python-formatter-keys)
      (when (and python-format-on-save
                 (eq 'black python-formatter))
        (add-hook 'python-mode-hook 'blacken-mode)))
    :config (spacemacs|hide-lighter blacken-mode)))

(defun python/post-init-eldoc ()
  (add-hook 'python-mode-local-vars-hook #'spacemacs//python-setup-eldoc))

(defun python/post-init-evil-matchit ()
  (add-hook `python-mode-hook `turn-on-evil-matchit-mode))

(defun python/post-init-flycheck ()
  (spacemacs/enable-flycheck 'python-mode))

(defun python/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'python-mode))

(defun python/post-init-ggtags ()
  (add-hook 'python-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun python/init-importmagic ()
  (use-package importmagic
    :defer t
    :commands (importmagic-mode importmagic-fix-symbol-at-point)
    :init
    (progn
      (add-hook 'python-mode-hook 'importmagic-mode)
      (spacemacs|diminish importmagic-mode " ⓘ" " [i]")
      (spacemacs/set-leader-keys-for-major-mode 'python-mode
        "rf" 'importmagic-fix-symbol-at-point))))

(defun python/init-live-py-mode ()
  (use-package live-py-mode
    :defer t
    :commands live-py-mode))

(defun python/init-nose ()
  (use-package nose
    :defer t
    :commands (nosetests-one
               nosetests-pdb-one
               nosetests-all
               nosetests-pdb-all
               nosetests-module
               nosetests-pdb-module
               nosetests-suite
               nosetests-pdb-suite)
    :config
    (progn
      (add-to-list 'nose-project-root-files "setup.cfg")
      (setq nose-use-verbose nil))))

(defun python/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(python . t))))

(defun python/init-pip-requirements ()
  (use-package pip-requirements
    :defer t))

(defun python/init-pippel ()
  (use-package pippel
    :defer t
    :config
    (evilified-state-evilify-map pippel-package-menu-mode-map
      :mode pippel-package-menu-mode)))

(defun python/init-py-isort ()
  (use-package py-isort
    :defer t
    :init (add-hook 'python-mode-hook
                    (lambda ()
                      (add-hook 'before-save-hook
                                'spacemacs//python-sort-imports
                                nil t)))))

(defun python/init-pyvenv ()
  (use-package pyvenv
    :defer t
    :init
    (progn
      (pcase python-auto-set-local-pyvenv-virtualenv
        (`on-visit
         (spacemacs/add-to-hooks 'spacemacs//pyvenv-mode-set-local-virtualenv
                                 '(python-mode-hook)))
        (`on-project-switch
         (add-hook 'projectile-after-switch-project-hook
                   'spacemacs//pyvenv-mode-set-local-virtualenv)))
      ;; setup shell correctly on environment switch
      (dolist (func '(pyvenv-activate pyvenv-deactivate pyvenv-workon))
        (advice-add func :after 'spacemacs/python-setup-everything)))))

(defun python/init-pylookup ()
  (use-package pylookup
    :defer t
    :commands (pylookup-lookup pylookup-update pylookup-update-all)
    :config
    (progn
      (evilified-state-evilify pylookup-mode pylookup-mode-map)
      (let ((dir (configuration-layer/get-layer-local-dir 'python)))
        (setq pylookup-dir (concat dir "pylookup/")
              pylookup-program (concat pylookup-dir "pylookup.py")
              pylookup-db-file (concat pylookup-dir "pylookup.db")))
      (setq pylookup-completing-read 'completing-read))))

(defun python/init-pytest ()
  (use-package pytest
    :defer t
    :commands (pytest-one
               pytest-pdb-one
               pytest-all
               pytest-pdb-all
               pytest-module
               pytest-pdb-module)
    :config (add-to-list 'pytest-project-root-files "setup.cfg")))

(defun python/init-python ()
  (use-package python
    :defer t
    :mode (("SConstruct\\'" . python-mode) ("SConscript\\'" . python-mode))
    :init
    (progn
      (spacemacs/register-repl 'python
                               'spacemacs/python-start-or-switch-repl "python")
      (spacemacs//bind-python-repl-keys)
      (spacemacs/add-to-hook 'python-mode-hook
                             '(spacemacs//python-setup-backend
                               spacemacs//python-default))
      ;; Toggle the indent guide
      (add-hook 'python-mode-hook (lambda ()
                                    (highlight-indentation-mode)
                                    (modify-syntax-entry ?_ "w")))

      ;; add support for `ahs-range-beginning-of-defun' for python-mode
      (with-eval-after-load 'auto-highlight-symbol
        (add-to-list 'ahs-plugin-bod-modes 'python-mode))

      ;; call `spacemacs//python-setup-shell' once, don't put it in a hook
      ;; (see issue #5988)
      (spacemacs//python-setup-shell))
    :config
    (progn
      ;; Set `python-indent-guess-indent-offset' to `nil' to prevent guessing `python-indent-offset
      ;; (we call python-indent-guess-indent-offset manually so python-mode does not need to do it)
      (setq-default python-indent-guess-indent-offset nil)

      ;; add this optional key binding for Emacs user, since it is unbound
      (define-key inferior-python-mode-map
        (kbd "C-r") 'comint-history-isearch-backward)
      ;; this key binding is for recentering buffer in Emacs
      ;; it would be troublesome if Emacs user
      ;; Vim users can use this key since they have other key
      (define-key inferior-python-mode-map
        (kbd "C-h") 'spacemacs/comint-clear-buffer))))

(defun python/post-init-stickyfunc-enhance ()
  (add-hook 'python-mode-hook 'spacemacs/load-stickyfunc-enhance))

(defun python/pre-init-xcscope ())

(defun python/init-yapfify ()
  (use-package yapfify
    :defer t
    :init
    (progn
      (spacemacs//bind-python-formatter-keys)
      (when (and python-format-on-save
                 (eq 'yapf python-formatter))
        (add-hook 'python-mode-hook 'yapf-mode)))
    :config (spacemacs|hide-lighter yapf-mode)))

(defun python/init-lsp-python-ms ()
  (use-package lsp-python-ms
    :if (eq python-lsp-server 'mspyls)
    :ensure nil
    :defer t
    :config

    (when python-lsp-git-root
      ;; Use dev version of language server checked out from github
      ;; init lsp-python-ms-extra-paths by environment variable PYTHONPATH
      (setq
       lsp-python-ms-extra-paths (split-string (getenv "PYTHONPATH") ":")
       lsp-python-ms-dir (expand-file-name (concat python-lsp-git-root "/output/bin/Release/"))
       lsp-python-executable-cmd "python"
       lsp-python-ms-executable (concat lsp-python-ms-dir "Microsoft.Python.LanguageServer.LanguageServer")
       )

      (message "lsp-python-ms: Using version at `%s'" lsp-python-ms-dir))
    ))
