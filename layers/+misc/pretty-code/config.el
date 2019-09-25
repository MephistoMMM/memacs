;;; config.el --- define configs for pretty-code
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;;; company-box
(setq company-box-doc-enable nil)

(defface memacs-company-box-backend-yasnippet-face
  '((t (:inherit font-lock-string-face)))
  "My company-box-backends-color for yasnippet"
  :group 'spacemacs)

(defface memacs-company-box-backend-yasnippet-selected-face
  '((t (:inherit company-box-selection)))
  "My company-box-backends-color for yasnippet select"
  :group 'spacemacs)

(setq company-box-backends-colors
      '((company-yasnippet . (:all
                              memacs-company-box-backend-yasnippet-face
                              :selected
                              memacs-company-box-backend-yasnippet-selected-face))
        ))

(setq company-box-icons-alist 'company-box-icons-all-the-icons)


;;;; all-the-icons
(setq all-the-icons-scale-factor 1)

;;; config.el ends here
