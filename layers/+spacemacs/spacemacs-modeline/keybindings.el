;;; keybindings.el --- define keybindings for spaceline layer
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;;; Spaceline toggles
(dolist (spec '((minor-modes "tmm")
                (major-mode "tmM")
                (version-control "tmv")
                (new-version "tmV")
                (point-position "tmp")
                (org-clock "tmc")))
  (let* ((segment (car spec))
         (status-var (intern (format "spaceline-%S-p" segment))))
    (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
             :status ,status-var
             :on (setq ,status-var t)
             :off (setq ,status-var nil)
             :documentation ,(format "Show %s in the mode-line."
                                     (replace-regexp-in-string
                                      "-" " " (format "%S" segment)))
             :evil-leader ,(cadr spec)))))


;;; keybindings.el ends here
