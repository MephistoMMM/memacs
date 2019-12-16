;;; funcs.el --- provide funcs for ein
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun ein:ml-lang-setup-go ()
  (when (featurep 'go-mode)
    (setq-local mode-name "EIN[GO]")
    (setq-local comment-start "# ")
    (setq-local comment-start-skip  "#+\\s-*")
    (setq-local indent-line-function
                (apply-partially #'ein:ml-indent-line-function #'go-mode-indent-line))
    (when (boundp 'go-mode-syntax-table)
      (set-syntax-table go-mode-syntax-table))
    (when (boundp 'go-mode-map)
      (set-keymap-parent ein:notebook-multilang-mode-map go-mode-map))))

;;; funcs.el ends here
