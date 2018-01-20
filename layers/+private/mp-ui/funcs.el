;;; mp-ui/funcs.el --- defines functions for ui

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: ui functions spacemacs elisp

;;; Commentary:

;; description

;;; Code:

;;;; Better-Default

(defun mp-ui/better-default ()
  "Provide better default for UI"
  ;; start line number
  (spacemacs/toggle-line-numbers-on)
  ;; start golden ratio
  ;; (spacemacs/toggle-golden-ratio-on)

  ;; Spaceline
  (setq ns-use-srgb-colorspace nil)

  ;; Parens
  (electric-pair-mode t)
  (show-paren-mode t)

  ;; Tab
  (setq tab-always-indent nil)

)

;;; mp-ui/funcs.el ends here
