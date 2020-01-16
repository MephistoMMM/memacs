;;; ui/workspaces/autoload/hydra.el -*- lexical-binding: t; -*-
;;;###if (featurep! :ui hydra)

(defvar +workspace-default-layout-name "main"
  "Previously selected layout.")

(defun +workspace/switch-default ()
  "Go to `+workspace-default-layout-name` layout"
  (interactive)
  (when +workspace-default-layout-name
    (+workspace-switch +workspace-default-layout-name)))


;;;###autoload (autoload 'doom-ts/workspace/body "ui/workspaces/autoload/hydra" nil t)
(define-hintable-transient-state! workspace
        :title "Workspaces Transient State"
        :doc
        "\n
 Go to^^^^^^                          Actions^^
 ─────^^^^^^───────────────────────── ───────^^───────────────────────────────
 [_1_.._9_]^^     nth/new workspace   [_a_]   new workspace
 [_C-1_.._C-9_]^^ nth/new workspace   [_D_]   close current workspace
 [_<tab>_]^^^^    last workspace      [_r_]   rename current workspace
 [_h_/_l_]^^      circle workspace    [_S_]   save all workspaces/save by names
 [_n_/_p_]^^      circle workspace    [_L_]   load workspaces from file
 [_b_]^^^^        buffer in workspace [_R_]   resote last session
 [_w_]^^^^        switch workspace    [_X_]   kill current session
 ^^^^^^                               [_q_]   quit
 ^^^^^^                               [_?_]   toggle help"
        :brief-format #'+workspace--tabline
        :bindings
          ;; need to exit in case number doesn't exist
        ("1" +workspace/switch-to-0 :exit t)
        ("2" +workspace/switch-to-1 :exit t)
        ("3" +workspace/switch-to-2 :exit t)
        ("4" +workspace/switch-to-3 :exit t)
        ("5" +workspace/switch-to-4 :exit t)
        ("6" +workspace/switch-to-5 :exit t)
        ("7" +workspace/switch-to-6 :exit t)
        ("8" +workspace/switch-to-7 :exit t)
        ("9" +workspace/switch-to-8 :exit t)
        ("C-1" +workspace/switch-to-0)
        ("C-2" +workspace/switch-to-1)
        ("C-3" +workspace/switch-to-2)
        ("C-4" +workspace/switch-to-3)
        ("C-5" +workspace/switch-to-4)
        ("C-6" +workspace/switch-to-5)
        ("C-7" +workspace/switch-to-6)
        ("C-8" +workspace/switch-to-7)
        ("C-9" +workspace/switch-to-8)
        ("<tab>" +workspace/other :exit t)
        ("<return>" nil :exit t)
        ("TAB" +workspace/other :exit t)
        ("RET" nil :exit t)
        ("a" +workspace/new :exit t)
        ("b" persp-switch-to-buffer :exit t)
        ("D" +workspace/delete :exit t)
        ("h" +workspace/switch-left)
        ("l" +workspace/switch-right)
        ("L" +workspace/load :exit t)
        ("n" +workspace/switch-right)
        ("p" +workspace/switch-left)
        ;; TODO: ("o" +layouts/select-custom-layout :exit t)
        ("r" +workspace/rename :exit t)
        ("R" +workspace/restore-last-session :exit t)
        ("S" +workspace/save :exit t)
        ("w" +workspace/switch-to :exit t)
        ("X" +workspace/kill-session :exit t)
        ("q" nil :exit t))

;; [_o_]^^^^        custom workspace
