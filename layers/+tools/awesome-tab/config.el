;;; config.el --- provide configs for awesome-tab
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(spacemacs/defer-until-after-user-config
 (lambda () (setq awesome-tab-active-color
             (face-attribute 'spacemacs-normal-face :background)
             awesome-tab-inactive-color
             (face-attribute 'font-lock-comment-face :foreground))))

(setq awesome-tab-separator (list 0.3))

;;; config.el ends here
