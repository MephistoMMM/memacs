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

(defun snails/pre-init-snails ( )
  (add-to-load-path (expand-file-name "~/Workspace/sources/snails"))
  )

(defun snails/init-snails ( )
  "Init snails. Details: https://github.com/manateelazycat/snails "
  (require 'snails)
  )

;;; packages.el ends here
