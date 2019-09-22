;;; keybindings.el --- provides keybindings for leetcode
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Labels the app as Leetcode se it doesn't appear as "prefix" in the menu
(spacemacs/declare-prefix "m l" "Leetcode")

;; The remaining useful keybindings to using Leetcode
(spacemacs/set-leader-keys
  "m l l" 'leetcode
  "m l d" 'leetcode-show-current-problem
  "m l r" 'leetcode-refresh
  "m l t" 'leetcode-try
  "m l u" 'leetcode-submit
  )

;;; keybindings.el ends here
