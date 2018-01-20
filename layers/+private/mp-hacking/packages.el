;;; mp-hacking/packages.el --- Defined packages and some base configs for mp-hacking.

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs layer elisp mp-hacking packages

;;; Commentary:

;;

;;; Code:
(setq mp-hacking-packages
      '(
        ;; leave-delimited
        (leave-delimited :location (recipe
                                    :fetcher github
                                    :repo "MephistoMMM/leave-delimited"))
        (goenv :location (recipe
                          :fetcher github
                          :repo "MephistoMMM/goenv"))
        ;; haskell-mode
        python
        ;; web-mode
        company
        js2-mode
        go-mode
        yaml-mode
        avy
        ))

(defun mp-hacking/post-init-avy ()
  "Bind keybindings for avy."
  (define-key evil-normal-state-map (kbd "C-j j") 'evil-avy-goto-char)
  (define-key evil-normal-state-map (kbd "C-j J") 'evil-avy-goto-char-2)
  (define-key evil-normal-state-map (kbd "C-j l") 'evil-avy-goto-line)
  (define-key evil-normal-state-map (kbd "C-j w") 'evil-avy-goto-word-or-subword-1)
  (define-key evil-normal-state-map (kbd "C-j W") 'evil-avy-goto-word-0)
  (define-key evil-normal-state-map (kbd "C-j t") 'evil-avy-goto-char-timer)

  ;; there is a bug with evil-search module
  ;; (define-key evil-normal-state-map (kbd "C-j i") 'avy-isearch)
  )

(defun mp-hacking/post-init-yaml-mode ()
  "Show linenum while opening yaml file."
  (add-hook 'yaml-mode-hook 'spacemacs/toggle-line-numbers-on)
  )

(defun mp-hacking/init-goenv ()
  "Add Goenv."
  (use-package goenv
    :defer t
    :init
    (with-eval-after-load 'go-mode
      (spacemacs/set-leader-keys-for-major-mode 'go-mode "Va" 'goenv-activate)
      (spacemacs/set-leader-keys-for-major-mode 'go-mode "Vd" 'goenv-deactivate))
    )
  )

(defun mp-hacking/post-init-go-mode ()
  "change flycheck-disabled-checkers"
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook (lambda ()
                                    (add-to-list 'flycheck-disabled-checkers 'go-vet)
                                    (add-to-list 'flycheck-disabled-checkers 'go-gofmt)
                                    (add-to-list 'flycheck-disabled-checkers 'go-errcheck)
                                    )))
  )

(defun mp-hacking/post-init-python ()
  "Toggle the indent guide"
    (add-hook 'python-mode-hook (lambda ()
                                  (indent-guide-mode)
                                  (modify-syntax-entry ?_ "w")))
  )

;; (defun mp-hacking/post-init-haskell-mode ()
;;   "Add haskell cabal bin path to 'exec-path."
;;   (with-eval-after-load 'haskell-mode
;;     (add-to-list 'exec-path
;;                  (concat (getenv "HOME") "/.cabal/bin/"))
;;     (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "=" 'mp-ui/format-haskell-buffer)
;;     )
;;   )

(defun mp-hacking/post-init-web-mode ()
  "Init web mode for indent offset, 'auto-mode-alist and so.
http://web-mode.org"
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (with-eval-after-load 'web-mode
    (setq web-mode-code-indent-offset 2)
    (setq web-mode-markup-indent-offset 2)
    (setq web-mode-css-indent-offset 2))
  )

(defun mp-hacking/post-init-company ()
  "Modify company-idle-delay to 0.6 .
TODO: we need a more elegant strategy."
  (add-hook 'company-mode-hook (lambda () (setq company-dabbrev-ignore-case t)))
  (with-eval-after-load 'company
    (setq company-idle-delay 0.6)))

(defun mp-hacking/init-leave-delimited ()
  "Let us be free to  go out of parens."
  (use-package leave-delimited
    :defer t))

;; (defun mp-hacking/post-init-flycheck ()
;;   "While flycheck enabled, Add hook for js2-mode to checking does js2-checks need to hide.."
;;   (add-hook 'js2-mode-hook 'mp-hacking/hide-js2-checks-if-flycheck-active))

(defun mp-hacking/post-init-js2-mode ()
  "Add a series of default configuration fo js2-mode"
  (add-hook 'js2-mode-hook '(lambda ()
                              (modify-syntax-entry ?_ "w")
                              (setq mode-name "JS2")
                              (company-mode)))

  (with-eval-after-load 'js2-mode
    (progn

      ;; these mode related variables must be in eval-after-load
      ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
      (setq-default js2-global-externs '("module"
                                         "require"
                                         "assert"
                                         "setTimeout"
                                         "clearTimeout"
                                         "setInterval"
                                         "clearInterval"
                                         "__dirname"
                                         "console"
                                         "JSON"))
      (setq-default js2-idle-timer-delay 0.2)
      (setq-default js-indent-level 2)
      (setq-default js2-basic-offset 2)

      ;; Let flycheck handle parse errors
      (setq-default js2-mode-show-parse-errors nil)
      (setq-default js2-mode-show-strict-warnings nil)
      (setq-default js2-highlight-external-variables t)
      (setq-default js2-strict-trailing-comma-warning nil)
    ))
  )

;;; packages.el ends here
