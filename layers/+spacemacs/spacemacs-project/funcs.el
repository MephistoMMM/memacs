;;; funcs.el --- Spacemacs Project Management Layer funcs File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com> & Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;;; projectile
(defun memacs/create-projectile-dot-file (dirname)
  "Create And Init .projectile file."
  (interactive "DProject Root Path: ")
  (let ((ξbuf (generate-new-buffer "Projectile File")))
    (switch-to-buffer ξbuf))
  (setq default-directory (file-name-directory dirname))
  (write-file (concat default-directory ".projectile"))
  )
