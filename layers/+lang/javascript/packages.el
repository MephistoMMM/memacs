;;; packages.el --- Javascript Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq javascript-packages
      '(
        add-node-modules-path
        evil-matchit
        flycheck
        ggtags
        counsel-gtags
        impatient-mode
        js-doc
        js2-mode
        js2-refactor
        json-mode
        json-snatcher
        (tern :toggle (spacemacs//tern-detect))
        web-beautify
        skewer-mode
        livid-mode
        ))

(defun javascript/post-init-add-node-modules-path ()
  (add-hook 'css-mode-hook #'add-node-modules-path)
  (add-hook 'js2-mode-hook #'add-node-modules-path)
  (add-hook 'json-mode-hook #'add-node-modules-path))

(defun javascript/post-init-flycheck ()
  (dolist (mode '(js2-mode json-mode))
    (spacemacs/enable-flycheck mode)))

(defun javascript/post-init-ggtags ()
  (add-hook 'js2-mode-local-vars-hook #'spacemacs/ggtags-mode-enable))

(defun javascript/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'js2-mode))

(defun javascript/post-init-impatient-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode "i" 'spacemacs/impatient-mode))

(defun javascript/init-js-doc ()
  (use-package js-doc
    :defer t
    :init (spacemacs/js-doc-set-key-bindings 'js2-mode)))

(defun javascript/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      ;; Required to make imenu functions work correctly
      (add-hook 'js2-mode-hook  (lambda ()
                                  (js2-imenu-extras-mode)
                                  (modify-syntax-entry ?_ "w")
                                  (setq mode-name "JS2"))))
    :config
    (progn
      ;; these mode related variables must be in eval-after-load
      ;; https://github.com/magnars/.emacs.d/blob/master/settings/setup-js2-mode.el
      (setq-default
       js2-global-externs '("module"
                            "require"
                            "assert"
                            "setTimeout"
                            "clearTimeout"
                            "setInterval"
                            "clearInterval"
                            "__dirname"
                            "console"
                            "JSON")
       js2-idle-timer-delay 0.2
       js-indent-level 2
       js2-basic-offset 2

       ;; Let flycheck handle parse errors
       js2-mode-show-parse-errors nil
       js2-mode-show-strict-warnings nil
       js2-highlight-external-variables t
       js2-strict-trailing-comma-warning nil)

      ;; prefixes
      (spacemacs/declare-prefix-for-mode 'js2-mode "h" "documentation")
      (spacemacs/declare-prefix-for-mode 'js2-mode "g" "goto")
      (spacemacs/declare-prefix-for-mode 'js2-mode "r" "refactor")
      (spacemacs/declare-prefix-for-mode 'js2-mode "z" "folding")
      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "w" 'js2-mode-toggle-warnings-and-errors
        "zc" 'js2-mode-hide-element
        "zo" 'js2-mode-show-element
        "zr" 'js2-mode-show-all
        "ze" 'js2-mode-toggle-element
        "zF" 'js2-mode-toggle-hide-functions
        "zC" 'js2-mode-toggle-hide-comments))))

(defun javascript/post-init-evil-matchit ()
  (add-hook `js2-mode `turn-on-evil-matchit-mode))

(defun javascript/init-js2-refactor ()
  (use-package js2-refactor
    :defer t
    :init
    (progn
      (add-hook 'js2-mode-hook 'spacemacs/js2-refactor-require)
      ;; prefixes
      (spacemacs/declare-prefix-for-mode 'js2-mode "r3" "ternary")
      (spacemacs/declare-prefix-for-mode 'js2-mode "ra" "add/args")
      (spacemacs/declare-prefix-for-mode 'js2-mode "rb" "barf")
      (spacemacs/declare-prefix-for-mode 'js2-mode "rc" "contract")
      (spacemacs/declare-prefix-for-mode 'js2-mode "re" "expand/extract")
      (spacemacs/declare-prefix-for-mode 'js2-mode "ri" "inline/inject/introduct")
      (spacemacs/declare-prefix-for-mode 'js2-mode "rl" "localize/log")
      (spacemacs/declare-prefix-for-mode 'js2-mode "rr" "rename")
      (spacemacs/declare-prefix-for-mode 'js2-mode "rs" "split/slurp")
      (spacemacs/declare-prefix-for-mode 'js2-mode "rt" "toggle")
      (spacemacs/declare-prefix-for-mode 'js2-mode "ru" "unwrap")
      (spacemacs/declare-prefix-for-mode 'js2-mode "rv" "var")
      (spacemacs/declare-prefix-for-mode 'js2-mode "rw" "wrap")
      (spacemacs/declare-prefix-for-mode 'js2-mode "x" "text")
      (spacemacs/declare-prefix-for-mode 'js2-mode "xm" "move")
      ;; key bindings
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "r3i" 'js2r-ternary-to-if
        "rag" 'js2r-add-to-globals-annotation
        "rao" 'js2r-arguments-to-object
        "rba" 'js2r-forward-barf
        "rca" 'js2r-contract-array
        "rco" 'js2r-contract-object
        "rcu" 'js2r-contract-function
        "rea" 'js2r-expand-array
        "ref" 'js2r-extract-function
        "rem" 'js2r-extract-method
        "reo" 'js2r-expand-object
        "reu" 'js2r-expand-function
        "rev" 'js2r-extract-var
        "rig" 'js2r-inject-global-in-iife
        "rip" 'js2r-introduce-parameter
        "riv" 'js2r-inline-var
        "rlp" 'js2r-localize-parameter
        "rlt" 'js2r-log-this
        "rrv" 'js2r-rename-var
        "rsl" 'js2r-forward-slurp
        "rss" 'js2r-split-string
        "rsv" 'js2r-split-var-declaration
        "rtf" 'js2r-toggle-function-expression-and-declaration
        "ruw" 'js2r-unwrap
        "rvt" 'js2r-var-to-this
        "rwi" 'js2r-wrap-buffer-in-iife
        "rwl" 'js2r-wrap-in-for-loop
        "k" 'js2r-kill
        "xmj" 'js2r-move-line-down
        "xmk" 'js2r-move-line-up))))

(defun javascript/init-json-mode ()
  (use-package json-mode
    :defer t))

(defun javascript/init-json-snatcher ()
  (use-package json-snatcher
    :defer t
    :config
    (spacemacs/set-leader-keys-for-major-mode 'json-mode
      "hp" 'jsons-print-path)))

(defun javascript/init-tern ()
  (use-package tern
    :defer t
    :init (add-hook 'js2-mode-hook 'tern-mode)
    :config
    (progn
      (spacemacs|hide-lighter tern-mode)
      (when javascript-disable-tern-port-files
        (add-to-list 'tern-command "--no-port-file" 'append))
      (spacemacs//set-tern-key-bindings 'js2-mode))))

(defun javascript/init-web-beautify ()
  (use-package web-beautify
    :defer t
    :init
    (progn
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "=" 'web-beautify-js)
      (spacemacs/set-leader-keys-for-major-mode 'json-mode
        "=" 'web-beautify-js)
      (spacemacs/set-leader-keys-for-major-mode 'web-mode
        "=" 'web-beautify-html)
      (spacemacs/set-leader-keys-for-major-mode 'css-mode
        "=" 'web-beautify-css))))

(defun javascript/init-skewer-mode ()
  (use-package skewer-mode
    :defer t
    :init
    (progn
      (spacemacs/register-repl 'skewer-mode
                               'spacemacs/skewer-start-repl
                               "skewer")
      (add-hook 'js2-mode-hook 'skewer-mode))
    :config
    (progn
      (spacemacs|hide-lighter skewer-mode)
      (spacemacs/declare-prefix-for-mode 'js2-mode "s" "skewer")
      (spacemacs/declare-prefix-for-mode 'js2-mode "e" "eval")
      (spacemacs/set-leader-keys-for-major-mode 'js2-mode
        "'" 'spacemacs/skewer-start-repl
        "ee" 'skewer-eval-last-expression
        "eE" 'skewer-eval-print-last-expression
        "sb" 'skewer-load-buffer
        "sB" 'spacemacs/skewer-load-buffer-and-focus
        "si" 'spacemacs/skewer-start-repl
        "sf" 'skewer-eval-defun
        "sF" 'spacemacs/skewer-eval-defun-and-focus
        "sr" 'spacemacs/skewer-eval-region
        "sR" 'spacemacs/skewer-eval-region-and-focus
        "ss" 'skewer-repl))))

(defun javascript/init-livid-mode ()
  (use-package livid-mode
    :defer t
    :init
    (progn
      (spacemacs|add-toggle javascript-repl-live-evaluation
        :mode livid-mode
        :documentation "Live evaluation of JS buffer change."
        :evil-leader-for-mode (js2-mode . "Tl"))
      (spacemacs|diminish livid-mode " 🅻" " [l]"))))
