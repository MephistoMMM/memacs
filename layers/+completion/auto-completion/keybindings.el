;;; config.el --- auto-completion Layer keybindings File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors & Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; auto-yasnippet
(spacemacs/set-leader-keys
  "sa" 'aya-create
  "se" 'spacemacs/auto-yasnippet-expand
  "sp" 'aya-persist-snippet)


;;; company
(with-eval-after-load 'company
  (define-key evil-hybrid-state-map (kbd "C-l") 'company-manual-begin)
  (let ((map company-active-map))
    (define-key map (kbd "C-n") 'company-select-next)
    (define-key map (kbd "C-p") 'company-select-previous)
    (define-key map (kbd "C-d") 'company-show-doc-buffer)))


;;; ivy-yasnippet
(memacs/define-insert-keybinding "s" 'spacemacs/ivy-yas)
(spacemacs/set-leader-keys "si" 'spacemacs/ivy-yas)


;;; yasnippet
;; Use `M-i s' to start complete snips and use TAB to go arround the points
;; How to write yasnippet: https://joaotavora.github.io/yasnippet/snippet-development.html or http://d.pr/n/1bHuv
(spacemacs/set-leader-keys
  "sn" 'yas-new-snippet
  "sl" 'yas-load-snippet-buffer
  "sc" 'yas-load-snippet-buffer-and-close
  "sv" 'yas-visit-snippet-file
  "sd" 'memacs/describe-yasnippets)
(spacemacs/defer-until-after-user-config
 (lambda ()
   (define-key yas-keymap [tab] nil)
   (define-key yas-keymap (kbd "TAB") nil)
   (define-key yas-keymap (kbd "C-l") 'yas-next-field-or-maybe-expand)
   ))
