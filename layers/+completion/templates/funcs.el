;;; funcs.el --- define functions for templates layer
;;
;; Copyright (c) 2015-2018 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun memacs//templates-switch-to-insert-mode-if-file-not-exits()
  "Switch evil mode to insert mode, if current file does not exit."
  (unless (file-exists-p (buffer-file-name)) (evil-insert 0)))

;;; funcs.el ends here
