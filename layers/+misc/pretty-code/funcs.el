;;; funcs.el --- provides functions for pretty-code
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun pretty-code//company-box-icons--tabnine (candidate)
  (print (text-properties-at 0 candidate))
  nil)

;;; funcs.el ends here
