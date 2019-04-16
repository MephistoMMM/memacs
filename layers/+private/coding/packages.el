;;; package.el --- packages configurations for leetcode and others
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst coding-packages
  '(
    dash
    furl
    graphql
    (leetcode :location (recipe
                         :fetcher github
                         :repo "kaiwk/leetcode.el"))
    )
  )

(defun coding/init-dash())
(defun coding/init-furl())
(defun coding/init-graphql())

(defun coding/init-leetcode()
  "Load leetcode"
  (use-package leetcode
    :commands (leetcode)
    :defer t
    :init
    (progn
      (setq leetcode-prefer-language "golang")
      (let ((accountfile (concat user-emacs-directory "LEETCODE")))
        (if (file-exists-p accountfile)
            (load accountfile)
          (message (format "WARNING: leetcode account file doesn't exist."))))
      ))
  )


;;; package.el ends here
