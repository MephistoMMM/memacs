;;; funcs.el --- Spacemacs editing Layer functions File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;; avy

(defun spacemacs/avy-goto-url()
  "Use avy to go to an URL in the buffer."
  (interactive)
  (avy-jump "https?://"))

(defun spacemacs/avy-open-url ()
  "Use avy to select an URL in the buffer and open it."
  (interactive)
  (save-excursion
    (spacemacs/avy-goto-url)
    (browse-url-at-point)))


;; uuidgen
;; TODO spacemacs/uuidgen-3 and spacemacs/uuidgen-5

(defun spacemacs/uuidgen-1 (arg)
  "Return a time based UUID (UUIDv1).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-1)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))

(defun spacemacs/uuidgen-4 (arg)
  "Return an UUID from random numbers (UUIDv4).
 If ARG is non nil then use CID format."
  (interactive "P")
  (let ((uuid (uuidgen-4)))
    (if arg
        (insert-uuid-cid uuid)
      (insert uuid))))


;; undo-tree

;; restore diff window after quit.  TODO fix upstream
(defun spacemacs/undo-tree-restore-default ()
  (setq undo-tree-visualizer-diff t))
