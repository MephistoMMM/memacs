;;; ui/hydra/config.el -*- lexical-binding: t; -*-

(use-package! hydra-examples
  :commands (hydra-move-splitter-up
             hydra-move-splitter-down
             hydra-move-splitter-right
             hydra-move-splitter-left))

;;;###package hydra
(setq lv-use-separator t)

(defadvice! +hydra--inhibit-window-switch-hooks-a (orig-fn)
  :around #'lv-window
  (let ((doom-inhibit-switch-window-hooks t))
    (funcall orig-fn)))

;; Prevent doom modeline to flash while open
(unless (featurep! modeline +light)
  (add-hook! doom-ts-enter #'+modeline-cancel-redisplay)
  (add-hook! doom-ts-exit #'+modeline-recover-redisplay))
