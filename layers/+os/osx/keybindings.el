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
      "Call `kbd' with a macOS-compatible Command-key (⌘) prefixed.
KEYS should be a string suitable as input to `kbd'.
`mac-commmand-modifier' determines which prefix will be added; it
should be set to one of `hyper', `meta', `super', or `alt'.  For example,
if KEYS is the string `f', it will be prefixed as `H-f', `s-f',
or `A-f' accordingly.  If KEYS is of the form `C-f', it likewise
will be prefixed as `H-C-f', `s-C-f', or `A-C-f'.

If `mac-command-modifier' is set to `none' or something other
than the three values listed above, `H-' will be used as the
default."
      (let ((found (assoc mac-command-modifier
                          '((hyper . "H-")
                            (meta  . "M-")
                            (super . "s-")
                            (alt   . "A-")))))
        (if found
            (kbd (concat (cdr found) keys))
          (kbd (concat "H-" keys)))))

    (global-set-key (kbd-mac-command "a") 'mark-whole-buffer)
    (global-set-key (kbd-mac-command "q") 'save-buffers-kill-terminal)
    (global-set-key (kbd-mac-command "v") 'yank)
    (global-set-key (kbd-mac-command "c") 'evil-yank)
    (global-set-key (kbd-mac-command "x") 'kill-region)
    (global-set-key (kbd-mac-command "s")
                    (lambda ()
                      (interactive)
                      (call-interactively (key-binding "\C-x\C-s"))))
    (global-set-key (kbd-mac-command "w") 'delete-window)
    (global-set-key (kbd-mac-command "W") 'delete-frame)
    (global-set-key (kbd-mac-command "n") 'make-frame)
    (global-set-key (kbd-mac-command "`") 'other-frame)
    (global-set-key (kbd-mac-command "z") 'undo-tree-undo)
    (global-set-key (kbd-mac-command "Z") 'undo-tree-redo)
    (global-set-key (kbd-mac-command "C-f") 'spacemacs/toggle-frame-fullscreen)
    ;; (global-set-key [(meta l)] 'hippie-expand)
    (spacemacs/set-leader-keys "f2" 'memacs/switch-to-item2-on-dir-of-current-buffer)
    (spacemacs/set-leader-keys "p2" 'memacs/switch-to-item2-on-dir-of-current-project)
    (global-set-key (kbd "M-s-h") 'ns-do-hide-others)
    )

  ;; C-v in ivy minibuffer
  (with-eval-after-load 'ivy
    (define-key ivy-minibuffer-map (kbd "M-v") 'yank))
  (memacs/define-search-keybinding "s" 'memacs/counsel-spotlight)
  )
