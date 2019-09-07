;;; packages.el --- provide packages for awesome-tab
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq awesome-tab-packages
      '(
        (awesome-tab :location (recipe
                                :fetcher github
                                :repo "MephistoMMM/awesome-tab"))
        ))

(defun awesome-tab/init-awesome-tab ()
  (use-package awesome-tab
    :commands (awesome-tab-mode)
    :defer t
    :init
    (progn
      (spacemacs|define-transient-state awesometab
        :title "Awesome-tab Transient State"
        :doc "
 Fast Move^^              Tab^^                  Search^^      Misc^^
 ───────^^────────────  ─────^^────────────────  ──────^^───── ─────^^──────────────
 [_p_/_n_] switch group [_C-a_/_C-e_] first/last [_b_] buffer  [_C-k_] kill buffer
 [_h_/_l_] switch tab   [_C-j_]^^ ace jump       [_g_] group   [_C-S-k_] kill others in group
 [_H_/_L_] switch other [_C-h_/_C-l_] move       ^^            ^^           [_q_] quit
"
        :on-enter (awesome-tab-mode t)
        :on-exit (awesome-tab-mode -1)
        :bindings
        ;; Fast Move
        ("p" awesome-tab-backward-group)
        ("n" awesome-tab-forward-group)
        ("h" awesome-tab-backward-tab)
        ("l" awesome-tab-forward-tab)
        ("H" awesome-tab-forward-tab-other-window)
        ("L" awesome-tab-backward-tab-other-window)
        ;; Tab
        ("C-a" awesome-tab-select-beg-tab)
        ("C-e" awesome-tab-select-end-tab)
        ("C-j" awesome-tab-ace-jump)
        ("C-h" awesome-tab-move-current-tab-to-left)
        ("C-l" awesome-tab-move-current-tab-to-right)
        ;; Search
        ("b" ivy-switch-buffer)
        ("g" awesome-tab-counsel-switch-group)
        ;; Misc
        ("C-k" kill-current-buffer)
        ("C-S-k" awesome-tab-kill-other-buffers-in-current-group)
        ("q" nil :exit t))
      (memacs/define-evil-normal-keybinding "C-t" 'spacemacs/awesometab-transient-state/body))
    :config
    (progn
      (set-face-attribute 'awesome-tab-selected nil
                          :foreground awesome-tab-active-color
                          :underline nil
                          :overline awesome-tab-active-color
                          )
      (set-face-attribute 'awesome-tab-unselected nil
                          :foreground awesome-tab-inactive-color
                          :underline awesome-tab-inactive-color
                          :overline nil
                          ))
    ))

;;; packages.el ends here
