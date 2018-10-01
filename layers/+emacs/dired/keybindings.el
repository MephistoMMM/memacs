;;; keybindings.el --- dired Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2014-2018 Sylvain Benner & Contributors
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; dired
(memacs/define-keys dired-mode-map
  ", o" 'memacs/dired-omit-and-remember
  ", i" 'memacs/dired-show-only
  ", f" 'memacs/dired-find-name-in-current-directory
  ", m" 'memacs/dired-open-mounted-media-dir
  ", s" 'memacs/dired-sort-and-remember
  "RET" 'memacs/dired-find-alternate-file
  "C-j" 'memacs/dired-find-alternate-file
  "^"   'memacs/dired-backward
  )
(spacemacs/set-leader-keys
  "jd" 'dired
  "fj" 'dired
  "md" 'dired-jump)


;; ranger
(spacemacs/set-leader-keys "ar" 'ranger)
