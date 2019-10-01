;;; keybindings.el --- define keybindings for spaceline layer
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;;; DoomLine toggles
(spacemacs|define-transient-state modeline
  :title "Modeline Transient State"
  :doc "
 Icon^^                 Segment^^         Buffer Style^^
 ───────────^^───────── ─────────^^────── ────────────^^──────────────── Given ~/Projects/FOSS/emacs/lisp/comint.el
 [_i_] display icon     [_M_] minor-mode  [_t u_] truncate-upto-project   ->  ~/P/F/emacs/lisp/comint.el
 [_m_] major-mode       [_W_] word-count  [_t f_] truncate-from-project   ->  ~/Projects/FOSS/emacs/l/comint.el
 [_c_] major-mode color [_E_] encoding    [_t w_] truncate-with-project   ->  emacs/l/comint.el
 [_s_] buffer-state     [_I_] indent-info [_t e_] truncate-except-project ->  ~/P/F/emacs/l/comint.el
 [_d_] modification     [_L_] lsp         [_t r_] truncate-upto-root      ->  ~/P/F/e/lisp/comint.el
 [_p_] perspective      [_P_] persp-name  [_t a_] truncate-all            ->  ~/P/F/e/l/comint.el
 ^^                     [_G_] github      [_r f_] relative-from-project   ->  emacs/lisp/comint.el
 ^^                     [_S_] checker     [_r t_] relative-to-project     ->  lisp/comint.el
 [_q_] exit             [_V_] env version [_b_]  buffer-name              ->  comint.el<2> (uniquify buffer name)"
  :bindings
  ;; Icon
  ("i" (setq doom-modeline-icon (not doom-modeline-icon))
   :toggle doom-modeline-icon)
  ("m" (setq doom-modeline-major-mode-icon (not doom-modeline-major-mode-icon))
   :toggle doom-modeline-major-mode-icon)
  ("c" (setq doom-modeline-major-mode-color-icon (not doom-modeline-major-mode-color-icon))
   :toggle doom-modeline-major-mode-color-icon)
  ("s" (setq doom-modeline-buffer-state-icon (not doom-modeline-buffer-state-icon))
   :toggle doom-modeline-buffer-state-icon)
  ("d" (setq doom-modeline-buffer-modification-icon (not doom-modeline-buffer-modification-icon))
   :toggle doom-modeline-buffer-modification-icon)
  ("p" (setq doom-modeline-persp-name-icon (not doom-modeline-persp-name-icon))
   :toggle doom-modeline-persp-name-icon)
  ;; Segment
  ("M" (setq doom-modeline-minor-modes (not doom-modeline-minor-modes))
   :toggle doom-modeline-minor-modes)
  ("W" (setq doom-modeline-enable-word-count (not doom-modeline-enable-word-count))
   :toggle doom-modeline-enable-word-count)
  ("E" (setq doom-modeline-buffer-encoding (not doom-modeline-buffer-encoding))
   :toggle doom-modeline-buffer-encoding)
  ("I" (setq doom-modeline-indent-info (not doom-modeline-indent-info))
   :toggle doom-modeline-indent-info)
  ("L" (setq doom-modeline-lsp (not doom-modeline-lsp))
   :toggle doom-modeline-lsp)
  ("P" (setq doom-modeline-persp-name (not doom-modeline-persp-name))
   :toggle doom-modeline-persp-name)
  ("G" (setq doom-modeline-github (not doom-modeline-github))
   :toggle doom-modeline-github)
  ("S" (setq doom-modeline-checker-simple-format (not doom-modeline-checker-simple-format))
   :toggle doom-modeline-checker-simple-format)
  ("V" (setq doom-modeline-env-version (not doom-modeline-env-version))
   :toggle doom-modeline-env-version)
  ;; Style
  ("t u" (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
   :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-project))
  ("t f" (setq doom-modeline-buffer-file-name-style 'truncate-from-project)
   :toggle (eq doom-modeline-buffer-file-name-style 'truncate-from-project))
  ("t w" (setq doom-modeline-buffer-file-name-style 'truncate-with-project)
   :toggle (eq doom-modeline-buffer-file-name-style 'truncate-with-project))
  ("t e" (setq doom-modeline-buffer-file-name-style 'truncate-except-project)
   :toggle (eq doom-modeline-buffer-file-name-style 'truncate-except-project))
  ("t r" (setq doom-modeline-buffer-file-name-style 'truncate-upto-root)
   :toggle (eq doom-modeline-buffer-file-name-style 'truncate-upto-root))
  ("t a" (setq doom-modeline-buffer-file-name-style 'truncate-all)
   :toggle (eq doom-modeline-buffer-file-name-style 'truncate-all))
  ("r f" (setq doom-modeline-buffer-file-name-style 'relative-from-project)
   :toggle (eq doom-modeline-buffer-file-name-style 'relative-from-project))
  ("r t" (setq doom-modeline-buffer-file-name-style 'relative-to-project)
   :toggle (eq doom-modeline-buffer-file-name-style 'relative-to-project))
  ("b" (setq doom-modeline-buffer-file-name-style 'buffer-name)
   :toggle (eq doom-modeline-buffer-file-name-style 'buffer-name))
  ;; other
  ("q" nil :exit t))

(spacemacs/set-leader-keys "tm" 'spacemacs/modeline-transient-state/body)

;;; keybindings.el ends here
