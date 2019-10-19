;;; memacs-themes-init.el --- provide initialization of memacs themes
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(require 'core-themes-support)
(require 'cl)

(defvar memacs-theme-configuration-file-name "config.el"
  "Configuration file name fo memacs-theme")

(setq memacs-theme--load-directory
      (file-name-directory load-file-name))

(defun memacs-theme//compare-theme-definition (a b)
  "Used as `compare-fn' for add-to-list."
  (when (and (listp a) (listp b))
    (equal
     (car a)
     (car b)))
  )

(defun memacs-theme/append-theme-definitions (themes)
  "Append theme definitions to spacemacs-theme-name-to-package"
  (loop for item in themes do
        (add-to-list 'spacemacs-theme-name-to-package item
                     nil #'memacs-theme//compare-theme-definition))
  )

(defun memacs-theme//init-load-theme-file (theme)
  "Default function to init theme file."
  (let* ((theme-name (symbol-name theme))
         (short-name (concat memacs-theme--load-directory theme-name ".el"))
         (verbose-name (concat memacs-theme--load-directory theme-name "-theme.el")))
    (if (file-exists-p short-name)
        (load-file short-name)
      (load-file verbose-name)
      ))
  )

;; load configuration
(load-file (concat memacs-theme--load-directory
                   memacs-theme-configuration-file-name))

(provide 'memacs-theme)

;;; memacs-themes-init.el ends here
