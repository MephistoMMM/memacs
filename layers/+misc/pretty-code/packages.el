;;; packages.el --- define packages for pettry-Code
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst pretty-code-packages
      '(
        (pretty-code :location local)
        ))

(defun pretty-code/init-pretty-code ()
  "Init pretty code."
  (spacemacs|do-after-display-system-init
    (use-package pretty-code
      :config
      (+pretty-code|setup-fira-ligatures)))
  )

;;; packages.el ends here
