;;; config.el --- OSX Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(when (spacemacs/system-is-mac)
  (when (display-graphic-p)

    ;; `Command' key is by default bound to HYPER (H-*),
    ;; `Option' key is by default bound to META (M-*).
    ;; `Function' key is by default not rebound.
    ;; `Control' key is by default not rebound.
    ;; The right variations of the above keys can
    ;; also be modified but are not rebound by
    ;; default.

    ;; `Alist' linking the layer config variables to
    ;; the internal Emacs variables for the modifier keys.
    (setq modifier-keys '((osx-command-as       . mac-command-modifier)
                          (osx-option-as        . mac-option-modifier)
                          (osx-function-as      . mac-function-modifier)
                          (osx-control-as       . mac-control-modifier)
                          (osx-right-command-as . mac-right-command-modifier)
                          (osx-right-option-as  . mac-right-option-modifier)
                          (osx-right-control-as . mac-right-control-modifier)))

    ;; The allowed non-nil values for the config variables.
    (setq allowed-values '(super meta hyper control alt none left))

    ;; Set internal variables according to the given config variables
    (cl-loop for (key-var . internal-var) in modifier-keys do
             (let ((key-value (symbol-value key-var)))
               (when (member key-value allowed-values)
                 (setf (symbol-value internal-var) key-value))))

    (defun kbd-mac-command (keys)
      "Wraps `kbd' function with Mac OSX compatible Command-key (⌘).
KEYS should be a string such as \"f\" which will be turned into values
such as \"H-f\", \"s-f\", or \"A-f\" depending on the value of
`mac-commmand-modifier' which could be `hyper', `super', or `alt'.
KEYS with a string of \"C-f\" are also valid and will be turned into
values such as \"H-C-f\".
Returns nil if `mac-command-modifier' is set to `none' or something
other than the three sane values listed above."
      (let ((found (assoc mac-command-modifier
                          '((hyper . "H-")
                            (meta  . "M-")
                            (super . "s-")
                            (alt   . "A-")))))
        (when found (kbd (concat (cdr found) keys)))))

    (global-set-key (kbd-mac-command "a") 'mark-whole-buffer)
    (global-set-key (kbd-mac-command "v") 'yank)
    (global-set-key (kbd-mac-command "c") 'kill-ring-save)
    (global-set-key (kbd-mac-command "x") 'kill-region)
    (global-set-key (kbd-mac-command "s") 'save-buffer)
    (global-set-key (kbd-mac-command "w") 'delete-window)
    (global-set-key (kbd-mac-command "W") 'delete-frame)
    (global-set-key (kbd-mac-command "`") 'other-frame)
    (global-set-key (kbd-mac-command "z") 'undo-tree-undo)
    (global-set-key (kbd-mac-command "Z") 'undo-tree-redo)
    (global-set-key (kbd-mac-command "C-f") 'spacemacs/toggle-frame-fullscreen)
    ;; (global-set-key [(meta l)] 'hippie-expand)
    (spacemacs/set-leader-keys "f2" 'memacs/switch-to-item2-on-dir-of-current-buffer)
    (global-set-key (kbd "M-s-h") 'ns-do-hide-others)
    )

  ;; C-v in ivy minibuffer
  (define-key ivy-minibuffer-map (kbd "M-v") 'yank)
  (memacs/define-search-keybinding "s" 'memacs/counsel-spotlight)
  )