;;; packages.el --- c-c++ layer keybindings file for Spacemacs.
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; c-c++

;;;; cc-mode
(dolist (mode c-c++-modes)
  (spacemacs/declare-prefix-for-mode mode "c" "compile")
  (spacemacs/declare-prefix-for-mode mode "g" "goto")
  (spacemacs/declare-prefix-for-mode mode "h" "hierarchy")
  (spacemacs/declare-prefix-for-mode mode "p" "project")
  (spacemacs/set-leader-keys-for-major-mode mode
    "ga" 'projectile-find-other-file
    "gA" 'projectile-find-other-file-other-window))

;;;; disaster
(dolist (mode c-c++-modes)
  (spacemacs/set-leader-keys-for-major-mode mode
    "D" 'disaster))

;;;; clang format
(dolist (mode c-c++-modes)
  (spacemacs/declare-prefix-for-mode mode "=" "format")
  (spacemacs/set-leader-keys-for-major-mode mode
    "==" 'spacemacs/clang-format-region-or-buffer
    "=f" 'spacemacs/clang-format-function))

;;;; xref
(dolist (mode c-c++-modes)
  (spacemacs/set-leader-keys-for-major-mode mode
    (kbd "gr") #'xref-find-references))

;;;; cquery
(dolist (mode c-c++-modes)
  (spacemacs/set-leader-keys-for-major-mode mode
    (kbd "hr") 'memacs/cquery-caller-hierarchy
    (kbd "he") 'memacs/cquery-callee-hierarchy
    (kbd "hb") 'memacs/cquery-base-inheritance-hierarchy
    (kbd "hd") 'memacs/cquery-derived-inheritance-hierarchy
    ))
