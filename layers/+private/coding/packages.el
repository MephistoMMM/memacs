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
    (setq leetcode-prefer-language "golang")
    :config
    (define-key leetcode--problems-mode-map (kbd "TAB") 'leetcode-show-current-problem)
    (define-key leetcode--problems-mode-map (kbd "<return>") 'leetcode-show-current-problem)
    )
  )


;;; package.el ends here
