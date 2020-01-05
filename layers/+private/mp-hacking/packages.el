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
        (beacon :location (recipe
                           :fetcher github
                           :repo "Malabarba/beacon"))
        ;; haskell-mode
        outshine
        (outline-ivy :location local)
        ))

(defun mp-hacking/init-beacon ()
  "Init beacon."
  (use-package beacon
    :init (spacemacs|diminish beacon-mode)
    :config (beacon-mode 1)))

(defun mp-hacking/init-outshine ()
  "Init outshine to manager code contents."
  (use-package outshine
    :defer t
    :init
    (progn
      (spacemacs|diminish outline-minor-mode)
      (spacemacs|diminish outshine-mode " ☰" " os")
      ;; Narrowing works within the headline rather than requiring to be on it
      (advice-add 'outshine-narrow-to-subtree :before
                  'mp-hacking//advise-outshine-narrow-start-pos)

      (add-hook 'outline-minor-mode-hook 'outshine-mode)
      (add-hook 'prog-mode-hook 'outline-minor-mode))))

(defun mp-hacking/init-outline-ivy ()
  (use-package outline-ivy
    :defer t
    :after ivy outshine))

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
