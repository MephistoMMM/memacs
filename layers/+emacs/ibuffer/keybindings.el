;;; keybindings.el --- ibuffer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(spacemacs/set-leader-keys "mb" 'ibuffer)
(global-set-key (kbd "C-x C-b") 'ibuffer)

(define-key ibuffer-mode-map (kbd "C-w") 'evil-window-map)
(spacemacs|transient-state-format-hint ibuffer
  memacs--ibuffer-ts-full-hint
  "\n
  Base^^^^                  Mark^^^^                   Operations^^^^
  ────^^^^───────────────── ────^^^^────────────────── ──────────^^^^───────────────
  [_RET_/_o_] buffer/window [_m_/_u_] mark/unmark      [_S_/_D_] (marked)SaveA/KillA            []~(￣▽￣)~*
  [_j_/_k_]   next/previous [_t_/_U_] toggle/unmarkA   [_J_/_K_] (marked)next/previous
  [_s_/_f_]   sort/filter   ^ ^ ^ ^                    [_?_]^ ^  toggle help
  [_r_]^ ^    flesh         [_%_/_*_] regexp/special   [_q_]^ ^  quit")
(spacemacs|define-transient-state ibuffer
  :title "IBUFFER HYDRA"
  :hint-is-doc t
  :dynamic-hint (memacs//ibuffer-ts-hint)
  :bindings
  ;; base
  ("?" memacs//ibuffer-ts-toggle-hint)
  ("q" nil :exit t)
  ("RET" ibuffer-visit-ibuffer)
  ("o" ibuffer-visit-buffer-other-window)
  ("r" ibuffer-update)
  ("j" next-line)
  ("k" previous-line)
  ;; mark
  ("m" ibuffer-mark-forward)
  ("u" ibuffer-unmark-forward)
  ("t" ibuffer-toggle-marks)
  ("U" ibuffer-unmark-all-marks :exit t)
  ("%" ibuffer-unmark-all-marks)
  ("*" ibuffer-unmark-all-marks)
  ("J" ibuffer-forward-next-marked)
  ("K" ibuffer-backwards-next-marked)
  ;; operations
  ("S" ibuffer-forward-next-marked)
  ("D" ibuffer-backward-next-marked)
  ("s" ibuffer-forward-next-marked)
  ("f" ibuffer-backwards-next-marked))
(memacs|open-ts-and-do ibuffer next-line)
(memacs|open-ts-and-do ibuffer previous-line)
(memacs|open-ts-and-do ibuffer ibuffer-mark-forward
                       (ibuffer-get-region-and-prefix))
;; TODO rewrite a more powerful macro to automatically gen
;;      enter functions and bind keys
(define-key ibuffer-mode-map (kbd "C-h") 'spacemacs/ibuffer-transient-state/body)
(define-key ibuffer-mode-map (kbd "?") 'memacs//ibuffer-ts-help)
(define-key ibuffer-mode-map (kbd "j") 'memacs/ibuffer-ts/next-line)
(define-key ibuffer-mode-map (kbd "k") 'memacs/ibuffer-ts/previous-line)
(define-key ibuffer-mode-map (kbd "m") 'memacs/ibuffer-ts/ibuffer-mark-forward)
