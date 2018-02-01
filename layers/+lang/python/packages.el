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
    eldoc
    evil-matchit
    flycheck
    importmagic
    live-py-mode
    (nose :location local)
    org
    pip-requirements
    pippel
    py-isort
    pyenv-mode
    (pylookup :location local)
    pytest
    (python :location built-in)
    pyvenv
    stickyfunc-enhance
    xcscope
    lsp-python
    ))

(defun python/post-init-eldoc ()
  (add-hook 'python-mode-hook 'spacemacs//init-eldoc-python-mode))

(defun python/post-init-evil-matchit ()
  (add-hook `python-mode-hook `turn-on-evil-matchit-mode))

(defun python/post-init-flycheck ()
  (spacemacs/enable-flycheck 'python-mode))

(defun python/init-importmagic ()
  (use-package importmagic
    :defer t
    :commands (importmagic-mode importmagic-fix-symbol-at-point)
    :init (add-hook 'python-mode-hook 'importmagic-mode)))

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
    :init (add-hook 'before-save-hook 'spacemacs//python-sort-imports)))

(defun python/init-pyenv-mode ()
  (use-package pyenv-mode
    :defer t
    :if (executable-find "pyenv")
    :commands (pyenv-mode-versions)
    :init
    (progn
      (pcase python-auto-set-local-pyenv-version
       (`on-visit
        (spacemacs/add-to-hooks 'spacemacs//pyenv-mode-set-local-version
                                '(python-mode-hook)))
       (`on-project-switch
        (add-hook 'projectile-after-switch-project-hook
                  'spacemacs//pyenv-mode-set-local-version)))
      ;; setup shell correctly on environment switch
      (dolist (func '(pyenv-mode-set pyenv-mode-unset))
        (advice-add func :after 'spacemacs/python-setup-everything))
      )))

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
      (spacemacs/register-repl 'python 'spacemacs/python-start-or-switch-repl "python")
      (add-hook 'inferior-python-mode-hook
                #'spacemacs//inferior-python-setup-hook)
      (add-hook 'python-mode-hook #'spacemacs//python-default)
      ;; Toggle the indent guide
      (add-hook 'python-mode-hook (lambda ()
                                    (highlight-indentation-mode)
                                    (modify-syntax-entry ?_ "w")))

      ;; add support for `ahs-range-beginning-of-defun' for python-mode
      (with-eval-after-load 'auto-highlight-symbol
        (add-to-list 'ahs-plugin-bod-modes 'python-mode))

      ;; call `spacemacs//python-setup-shell' once, don't put it in a hook
      ;; (see issue #5988)
      (spacemacs//python-setup-shell))))

(defun python/post-init-stickyfunc-enhance ()
  (add-hook 'python-mode-hook 'spacemacs/load-stickyfunc-enhance))

(defun python/pre-init-xcscope ())

(defun python/init-lsp-python ()
  (use-package lsp-python
    :defer t
    :commands (lsp-python-enable)
    :init
    (progn
      (add-hook 'python-mode-hook #'lsp-python-enable)

      ;; add company lsp
      (spacemacs|add-company-backends :backends company-lsp :modes python-mode)
      (when python-enable-lsp-format-on-save
        (add-hook 'python-mode-hook 'spacemacs//add-python-format-on-save))
      )))
