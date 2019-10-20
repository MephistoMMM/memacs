;;; funcs.el --- provide functions for pretty-code Layer
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun pretty-code//unicode-setup-fonts-h (&optional frame)
  "Initialize  `unicode-fonts', if in a Gui session"
  (when (and frame (display-graphic-p frame))
    (with-selected-frame
        (require 'unicode-fonts)
      ;; NOTE will impact startup time on first run
      (unicode-fonts-setup))))

(eval-and-compile
  ;; Icons selected by liguangsheng
  ;; https://github.com/liguangsheng/emacsd/blob/master/lisp/init-completion.el
  (defun memacs/company-box-icon (family icon &rest args)
    "Defines icons using `all-the-icons' for `company-box'."
    (when icon
      (let ((icon (pcase family
                    ('octicon (all-the-icons-octicon icon :height 0.8 :v-adjust -0.05 args))
                    ('faicon (all-the-icons-faicon icon :height 0.8 :v-adjust -0.0575))
                    ('material (all-the-icons-material icon :height 0.8 :v-adjust -0.225 args))
                    ('alltheicon (all-the-icons-alltheicon icon :height 0.8 args)))))
        (unless (symbolp icon)
          (concat icon
                  (propertize " " 'face 'variable-pitch)))))))

;;; funcs.el ends here
