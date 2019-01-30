;;; funcs.el --- define functions
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun color-rg/slim-color-rg-transient-state/body (&optional keyword directory files)
  "Open slim color-rg hydra body."
  (color-rg-search-input keyword directory files)
  (spacemacs/slim-color-rg-transient-state/body))

;;; funcs.el ends here
