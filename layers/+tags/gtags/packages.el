;;; packages.el --- gtags Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;;    and: Christian E. Hopps <chopps@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst gtags-packages
  '(
    ggtags
    (counsel-gtags :requires ivy)
    ))

(defun gtags/init-counsel-gtags ()
  (use-package counsel-gtags
    :defer t
    :init
    (progn
      (setq counsel-gtags-ignore-case t
            counsel-gtags-auto-update t)
      ;; modes that do not have a layer, define here
      (spacemacs/counsel-gtags-define-keys-for-mode 'tcl-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'vhdl-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'awk-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'dired-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'compilation-mode)
      (spacemacs/counsel-gtags-define-keys-for-mode 'shell-mode))))

(defun gtags/init-ggtags ()
  (use-package ggtags
    :defer t
    :init
    (progn
      ;; modes that do not have a layer, add here.
      (add-hook 'awk-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
      (add-hook 'shell-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
      (add-hook 'tcl-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
      (add-hook 'vhdl-mode-local-vars-hook #'spacemacs/ggtags-mode-enable)
      (spacemacs|add-toggle ggtags-mode
        :status ggtags-mode
        :on (ggtags-mode nil)
        :off (ggtags-mode -1)
        :documentation "Toggle GNU Global source code tagging system."
        ;; TODO make a list of all supported major modes by ggtags, check
        ;; how it is done with evil-cleverparens
        ;; TODO update the documentation in each supported layers like it
        ;; has been done with evil-cleverparens and lisp dialects
        :evil-leader-for-mode
        (asm-mode . "Tg")
        (awk-mode . "Tg")
        (c-mode . "Tg")
        (c++-mode . "Tg")
        (clojure-mode . "Tg")
        (common-lisp-mode . "Tg")
        (compilation-mode . "Tg")
        (csharp-mode . "Tg")
        (d-mode . "Tg")
        (dired-mode . "Tg")
        (dos-mode . "Tg")
        (elixir-mode . "Tg")
        (emacs-lisp-mode . "Tg")
        (erlang-mode . "Tg")
        (fsharp-mode . "Tg")
        (go-mode . "Tg")
        (haskell-mode . "Tg")
        (java-mode . "Tg")
        (js2-mode . "Tg")
        (latex-mode . "Tg")
        (lua-mode . "Tg")
        (ocaml-mode . "Tg")
        (octave-mode . "Tg")
        (php-mode . "Tg")
        (python-mode . "Tg")
        (racket-mode . "Tg")
        (ruby-mode . "Tg")
        (rust-mode . "Tg")
        (scala-mode . "Tg")
        (scheme-mode . "Tg")
        (sh-mode . "Tg")
        (shell-mode . "Tg")
        (tcl-mode . "Tg")
        (vhdl-mode . "Tg")
        (vimrc-mode . "Tg")))
    :config
    (progn (spacemacs|diminish ggtags-mode " 🅶" " [g]"))))
