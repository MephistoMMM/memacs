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
        dap-mode
        evil-matchit
        flycheck
        counsel-gtags
        company
        impatient-mode
        import-js
        js-doc
        js2-mode
        js2-refactor
        livid-mode
        org
        skewer-mode
        tern
        web-beautify
        ))

(defun javascript/post-init-add-node-modules-path ()
  (spacemacs/add-to-hooks #'add-node-modules-path '(css-mode-hook
                                                    js2-mode-hook)))

(defun javascript/post-init-company ()
  (add-hook 'js2-mode-local-vars-hook #'spacemacs//javascript-setup-company))

(defun javascript/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'js2-mode))

(defun javascript/pre-init-dap-mode ()
  (add-to-list 'spacemacs--dap-supported-modes 'js2-mode)
  (add-hook 'js2-mode-local-vars-hook #'spacemacs//javascript-setup-dap))

(defun javascript/post-init-evil-matchit ()
  (add-hook `js2-mode-hook `turn-on-evil-matchit-mode))

(defun javascript/post-init-flycheck ()
  (spacemacs/enable-flycheck 'js2-mode)
  (add-hook 'js2-mode-hook #'spacemacs//javascript-setup-checkers 'append))

(defun javascript/post-init-counsel-gtags ()
  (spacemacs/counsel-gtags-define-keys-for-mode 'js2-mode))

(defun javascript/post-init-impatient-mode ()
  (spacemacs/set-leader-keys-for-major-mode 'js2-mode
    "I" 'spacemacs/impatient-mode))

(defun javascript/pre-init-import-js ()
  (when (eq javascript-import-tool 'import-js)
    (add-to-list 'spacemacs--import-js-modes (cons 'js2-mode 'js2-mode-hook))))

(defun javascript/init-js-doc ()
  (use-package js-doc
    :defer t
    :init (spacemacs/js-doc-set-key-bindings 'js2-mode)))

(defun javascript/init-js2-mode ()
  (use-package js2-mode
    :defer t
    :mode (("\\.m?js\\'"  . js2-mode))
    :init
    (progn
      (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
      ;; Required to make imenu functions work correctly
      (add-hook 'js2-mode-hook  (lambda ()
                                  (js2-imenu-extras-mode)
                                  (modify-syntax-entry ?_ "w")
                                  (setq mode-name "JS2"))))
    (add-hook 'js2-mode-local-vars-hook
              #'spacemacs//javascript-setup-backend)
    (add-hook 'js2-mode-local-vars-hook
              #'spacemacs//javascript-setup-next-error-fn)
    ;; safe values for backend to be used in directory file variables
    (dolist (value '(lsp tern))
      (add-to-list 'safe-local-variable-values
                   (cons 'javascript-backend value)))
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

      (when javascript-fmt-on-save
        (add-hook 'js2-mode-local-vars-hook 'spacemacs/javascript-fmt-before-save-hook))
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

(defun javascript/init-livid-mode ()
  (when (eq javascript-repl 'skewer)
    (use-package livid-mode
      :defer t
      :init
      (progn
        (spacemacs/declare-prefix-for-mode 'js2-mode "T" "toggle")
        (spacemacs|add-toggle javascript-repl-live-evaluation
          :mode livid-mode
          :documentation "Live evaluation of JS buffer change."
          :evil-leader-for-mode (js2-mode . "Tl"))
        (spacemacs|diminish livid-mode " 🅻" " [l]")))))

(defun javascript/pre-init-org ()
  (spacemacs|use-package-add-hook org
    :post-config (add-to-list 'org-babel-load-languages '(js . t))))

(defun javascript/pre-init-prettier-js ()
  (when (eq javascript-fmt-tool 'prettier)
    (add-to-list 'spacemacs--prettier-modes 'js2-mode)))

(defun javascript/init-skewer-mode ()
  (when (eq javascript-repl 'skewer)
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
          "ss" 'skewer-repl)))))

(defun javascript/post-init-tern ()
  (add-to-list 'tern--key-bindings-modes 'js2-mode))

(defun javascript/pre-init-web-beautify ()
  (add-to-list 'spacemacs--web-beautify-modes (cons 'js2-mode 'web-beautify-js)))
