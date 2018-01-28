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
        outshine
        ))

(defun mp-hacking/init-outshine ()
  "Bind outshine to SPE o o"
  (use-package outshine
    :defer t
    :init
    (spacemacs|diminish outline-minor-mode " ♗" " @")
    (add-hook 'outline-minor-mode-hook 'outshine-hook-function)
    (add-hook 'prog-mode-hook 'outline-minor-mode)
    (advice-add 'outshine-narrow-to-subtree :before
                (lambda (&rest args) (unless (outline-on-heading-p t)
                                       (outline-previous-visible-heading 1))))

    (spacemacs/declare-prefix "oo" "outshine")
    ;; Keybinding
    (spacemacs/set-leader-keys
      ;; Insert
      "ooi" 'outshine-insert-heading
      "oob" 'outshine-cycle-buffer

      ;; Narrowing
      "oon" 'outshine-narrow-to-subtree
      "oow" 'widen

      ;; Structural edits and moves
      "ooj" 'outline-forward-same-level
      "ook" 'outline-backward-same-level
      "ooh" 'outline-up-heading
      "ool" 'outline-next-visible-heading
      "oou" 'outline-previous-visible-heading
      "ooJ" 'outline-move-subtree-down
      "ooK" 'outline-move-subtree-up
      "ooH" 'outline-promote
      "ooL" 'outline-demote
      )
    )
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

;; (defun mp-hacking/post-init-haskell-mode ()
;;   "Add haskell cabal bin path to 'exec-path."
;;   (with-eval-after-load 'haskell-mode
;;     (add-to-list 'exec-path
;;                  (concat (getenv "HOME") "/.cabal/bin/"))
;;     (spacemacs/set-leader-keys-for-major-mode 'haskell-mode "=" 'mp-ui/format-haskell-buffer)
;;     )
;;   )

(defun mp-hacking/init-leave-delimited ()
  "Let us be free to  go out of parens."
  (use-package leave-delimited
    :defer t))

;;; packages.el ends here
