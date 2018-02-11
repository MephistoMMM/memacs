;;; memacs/config.el --- define configs for memacs layer

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keywords

;;; Code:


;; Spaceline

(setq memacs-spaceline-left-segments
      `(
        ((((persp-name workspace-number) :separator "|")
          buffer-modified)
         :fallback evil-state
         :face highlight-face
         :priority 0)
        ((buffer-size buffer-id remote-host)
         :priority 5)
        auto-comple
        major-mode
        (process :when active)
        (minor-modes :when active)
        (mu4e-alert-segment :when active)
        (erc-track :when active)
        (version-control :when active
                         :priority 7)
        (org-pomodoro :when active)
        (org-clock :when active)))

(setq memacs-spaceline-right-segments
      `(
        which-function
        (python-pyvenv :fallback python-pyenv)
        (selection-info :priority 2)
        input-method
        ((buffer-encoding-abbrev
          point-position
          line-column)
         :separator " | "
         :priority 3)
        ((flycheck-error flycheck-warning flycheck-info)
         :when active
         :priority 3)
        (global :when active)
        (buffer-position :priority 0)
        (hud :priority 0)))

;;; memacs/config.el ends here
