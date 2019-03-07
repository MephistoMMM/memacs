;;; config.el --- Ivy Layer Configuration File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Base
(setq swiper-action-recenter t)
(setq ivy-count-format "(%d/%d) ")
(setq ivy-on-del-error-function nil)
(setq counsel-find-file-at-point t)
(setq counsel-yank-pop-separator "\n-------\n")
(setq ivy-format-function 'memacs//ivy-format-function-arrow)


;; Layer Variables

(defvar ivy-enable-advanced-buffer-information nil
  "If non-nil, enable `ivy-rich' which adds information on buffers.")


;; Private Variables

(defvar spacemacs--counsel-commands
  '(;; --line-number forces line numbers (disabled by default on windows)
    ;; no --vimgrep because it adds column numbers that wgrep can't handle
    ;; see https://github.com/syl20bnr/spacemacs/pull/8065
    ("rg" . "rg --smart-case --no-heading --color never --line-number --max-columns 150 %s %S .")
    ("ag" . "ag --nocolor --nogroup %s %S .")
    ("grep" . "grep -nrP %s %S ."))
  "An alist of search commands and their corresponding commands
with options to run in the shell.")

(defvar spacemacs--counsel-search-max-path-length 30
  "Truncate the current path in counsel search if it is longer
than this amount.")

(defvar spacemacs--counsel-initial-number-cand 100)

(defvar memacs--ivy-file-actions
  '(("x" find-file-other-frame "other frame")
    ("j" find-file-other-window "other window")
    ("v" spacemacs/find-file-vsplit "in vertical split")
    ("s" spacemacs/find-file-split "in horizontal split")
    ("l" find-file-literally "literally")
    ("d" spacemacs/delete-file-confirm "delete file")
    ("R" spacemacs/rename-file "rename file"))
  "Default ivy actions for files.")

(defvar memacs--ivy-projectile-actions
  '(("v" memacs/projectile-find-file-vsplit "in vertical split")
    ("s" memacs/projectile-find-file-split "in horizontal split")
    ("l" memacs/projectile-find-file-literally "literally")
    ("d" memacs/projectile-delete-file-confirm "delete file")
    ("R" memacs/projectile-rename-file "rename file"))
  "Default ivy actions for projectile.")

(defvar memacs--ivy-grep-actions
  (cl-loop for j in memacs--ivy-file-actions
           for key = (nth 0 j)
           for func = (nth 1 j)
           for desc = (nth 2 j)
           collect `(,key (lambda (x) (spacemacs//counsel-with-git-grep (quote ,func) x)) ,desc))
  "Default ivy actions to be used with git-grep output.")


;; ivy rich

(setq ivy-rich--display-transformers-list
      '(ivy-switch-buffer
        (:columns
         ((ivy-rich-buffer-icon (:width 1))
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))
        ivy-switch-buffer-other-window
        (:columns
         ((ivy-rich-buffer-icon)
          (ivy-rich-candidate (:width 30))
          (ivy-rich-switch-buffer-size (:width 7))
          (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
          (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
          (ivy-rich-switch-buffer-project (:width 15 :face success))
          (ivy-rich-switch-buffer-path (:width (lambda (x) (ivy-rich-switch-buffer-shorten-path x (ivy-rich-minibuffer-width 0.3))))))
         :predicate
         (lambda (cand) (get-buffer cand)))
        counsel-M-x
        (:columns
         ((counsel-M-x-transformer (:width 50))
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
        counsel-describe-function
        (:columns
         ((counsel-describe-function-transformer (:width 50))
          (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
        counsel-describe-variable
        (:columns
         ((counsel-describe-variable-transformer (:width 50))
          (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
        counsel-find-file
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-file-jump
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-dired-jump
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-git
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-projectile-find-file
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-projectile-find-dir
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 30))))
        counsel-recentf
        (:columns
         ((ivy-rich-file-icon)
          (ivy-rich-candidate (:width 90))
          (ivy-rich-file-last-modified-time (:face font-lock-comment-face))))
        counsel-bookmark
        (:columns
         ((ivy-rich-bookmark-type)
          (ivy-rich-bookmark-name (:width 40))
          (ivy-rich-bookmark-info)))
        ))
