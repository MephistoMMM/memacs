;;; packages.el --- define packages and init them
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst color-rg-packages
  '((color-rg :location (recipe
                         :fetcher github
                         :repo "manateelazycat/color-rg"))
    ))

(defun color-rg/init-color-rg ()
  "Search and refactoring tool based on ripgrep.
https://github.com/manateelazycat/color-rg"
  (use-package color-rg
    :defer t
    :commands (color-rg-search-input color-rg-read-input)
    :init
    (progn
      (spacemacs|define-transient-state slim-color-rg
        :title "Slim color-rg Transient State"
        :doc "
 Move^^                   Toggle^^         Research^^     Other^^
───────^^───────────────  ─────^^───────  ─────^^───────  ─────^^───────────
 [_n_/_p_] next/pre        [_i_] ignore   [_s_] regexp    [_RET_] open file
 [_N_/_P_] next/pre file   [_c_] case     [_l_] literal   [_q_]   quit
"
        :bindings
        ;; Move
        ("n" color-rg-jump-next-keyword)
        ("p" color-rg-jump-prev-keyword)
        ("N" color-rg-jump-next-file)
        ("P" color-rg-jump-prev-file)
        ;; Research
        ("s" color-rg-rerun-regexp)
        ("l" color-rg-rerun-literal)
        ;; Toggle
        ("i" color-rg-rerun-toggle-ignore)
        ("c" color-rg-rerun-toggle-case)
        ;; Other
        ("RET" color-rg-open-file)
        ("q" (ignore-errors (color-rg-quit)) :exit t)
        )
      )
    :config
    (progn
      (dolist (keydata '(("?" . nil )
                         ("C-h" . nil )))
        (evil-define-key 'normal color-rg-mode-map (kbd (car keydata))
          'spacemacs/slim-color-rg-transient-state/body))

      (spacemacs/set-leader-keys-for-major-mode 'color-rg-mode
        "," 'spacemacs/slim-color-rg-transient-state/body)
      )
    )
  )


;;; packages.el ends here
