;;; packages.el --- CMake layer packages  fuke for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Alexander Dalshov <dalshov@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(setq cmake-packages
      '(
        cmake-ide
        cmake-mode
        company
        ))

(defun cmake/init-cmake-ide ()
  (use-package cmake-ide
    :if cmake-enable-cmake-ide-support
    :defer t
    :init (spacemacs/add-to-hooks 'cmake-ide--mode-hook '(c-mode-hook
                                                          c++-mode-hook))
    :config
    (progn
      (cmake-ide-setup)
      (dolist (mode cmake-modes)
        (spacemacs/declare-prefix-for-mode mode "c" "compile")
        (spacemacs/declare-prefix-for-mode mode "p" "project")
        (spacemacs/set-leader-keys-for-major-mode mode
          "cc" 'cmake-ide-compile
          "pc" 'cmake-ide-run-cmake
          "pC" 'cmake-ide-maybe-run-cmake
          "pd" 'cmake-ide-delete-file)))))

(defun cmake/init-cmake-mode ()
  (use-package cmake-mode
    :defer t
    :mode (("CMakeLists\\.txt\\'" . cmake-mode) ("\\.cmake\\'" . cmake-mode))))

(defun cmake/post-init-company ()
  (when (configuration-layer/package-used-p 'cmake-mode)
    (spacemacs|add-company-backends :backends company-cmake :modes cmake-mode)))

