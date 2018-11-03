;;; config.el --- OSX Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(when (spacemacs/system-is-mac)
  (global-set-key [(meta a)] 'mark-whole-buffer)
  (global-set-key [(meta v)] 'yank)
  (global-set-key [(meta c)] 'kill-ring-save)
  (global-set-key [(meta s)] 'save-buffer)
  (global-set-key [(meta w)] 'delete-frame)
  (global-set-key [(meta z)] 'undo)
  ;; (global-set-key [(meta l)] 'hippie-expand)
  ;; C-v in ivy minibuffer
  (define-key ivy-minibuffer-map (kbd "M-v") 'yank)
  (spacemacs/set-leader-keys "f2" 'memacs/switch-to-item2-on-dir-of-current-buffer)
  (memacs/define-search-keybinding "s" 'memacs/counsel-spotlight)
  )
