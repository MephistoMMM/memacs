;;; ui/hydra/config.el -*- lexical-binding: t; -*-

(use-package! hydra-examples
  :commands (hydra-move-splitter-up
             hydra-move-splitter-down
             hydra-move-splitter-right
             hydra-move-splitter-left))

;;;###package hydra
(setq lv-use-separator t)

(defadvice! +hydra--inhibit-window-switch-hooks-a (fn)
  :around #'lv-window
  (let (doom-switch-window-hook)
    (funcall fn)))

;; Prevent doom modeline to flash while open
(unless (featurep! modeline +light)
  (add-hook! doom-ts-enter #'+modeline-cancel-redisplay)
  (add-hook! doom-ts-exit #'+modeline-recover-redisplay))
