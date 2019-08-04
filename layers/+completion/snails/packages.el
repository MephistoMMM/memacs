;;; packages.el --- provide packages about snails
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst snails-packages
  '(
    exec-path-from-shell
    (snails :location built-in)
    ))

(defun snails/init-exec-path-from-shell() )

(defun snails/init-snails ( )
  "Init snails. Details: https://github.com/manateelazycat/snails "
  (let ((absolute-load-path
         (expand-file-name "~/Workspace/sources/snails")))
    (when (file-directory-p absolute-load-path)
      (add-to-load-path absolute-load-path)
      (require 'snails))
    )
  )

;;; packages.el ends here
