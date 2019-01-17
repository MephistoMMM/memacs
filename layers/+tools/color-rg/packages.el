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
 Move^^^^^^^^              Search^^^^              Filter^^          Other^^^^
─────^^^^^^^^───────────  ───────^^^^───────────  ───────^^───────  ─────^^^^───────
 [_n_|_j_/_p_|_k_] line   [_s_/_l_] re/literal     [_f_] match      [_u_] unfilter
 [_N_|_J_/_P_|_K_] file   [_i_/_c_] ignore/case    [_F_] mismatch   [_RET_/_q_] open/quit
"
        :bindings
        ;; Move
        ("n" color-rg-jump-next-keyword)
        ("p" color-rg-jump-prev-keyword)
        ("N" color-rg-jump-next-file)
        ("P" color-rg-jump-prev-file)
        ("j" color-rg-jump-next-keyword)
        ("k" color-rg-jump-prev-keyword)
        ("J" color-rg-jump-next-file)
        ("K" color-rg-jump-prev-file)
        ;; Research
        ("s" color-rg-rerun-regexp)
        ("l" color-rg-rerun-literal)
        ;; Toggle
        ("i" color-rg-rerun-toggle-ignore)
        ("c" color-rg-rerun-toggle-case)
        ;; Filter
        ("f" color-rg-filter-match-results)
        ("F" color-rg-filter-mismatch-results)
        ("u" color-rg-unfilter)
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
