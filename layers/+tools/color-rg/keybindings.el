;;; keybindings.el --- define keybindings for color-rg
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

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

(spacemacs/set-leader-keys-for-major-mode 'color-rg-mode
  "," 'spacemacs/slim-color-rg-transient-state/body)

;;; keybindings.el ends here
