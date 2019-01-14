;;; packages.el --- define packages and init them
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst color-rg-packages
  '((color-rg :location (recipe
                         :fetcher github
                         :repo "manateelazycat/color-rg"))
    ))

(defun color-rg/init-color-rg ()
  "Search and refactoring tool based on ripgrep.
https://github.com/manateelazycat/color-rg"
  (use-package color-rg
    :defer t
    :commands (color-rg-search-input))
  )


;;; packages.el ends here
