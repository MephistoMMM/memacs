;;; config.el --- configurations for themes
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


(defun memacs-theme//init-doom-theme (theme)
  (add-to-load-path (concat memacs-theme--load-directory "doom-theme"))
  (require 'doom-themes))

(defun memacs-theme//init-tao-theme (theme)
  (add-to-load-path (concat memacs-theme--load-directory "tao-theme"))
  (load-file (concat memacs-theme--load-directory
                     "tao-theme/" (symbol-name theme-name) "-theme.el")))

(memacs-theme/append-theme-definitions
 '(
   (doom-one . memacs-theme//init-doom-theme)
   (purifier . memacs-theme//init-load-theme-file)
   (dracula  . memacs-theme//init-load-theme-file)
   (tao-yin  . memacs-theme//init-tao-theme)
   (tao-yang . memacs-theme//init-tao-theme)
   ))

;;; config.el ends here
