;;; packages.el --- define packages for pettry-Code
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst pretty-code-packages
  '(
    (pretty-code :location local)
    (font-lock+ :location local)
    (icons-in-terminal :location local)

    (company-box :requires company)
    ))

(defun pretty-code/init-font-lock+()
  "Init font-lock+.")

(defun pretty-code/init-icons-in-terminal()
  "Init icons-in-terminal.")

(defun pretty-code/init-company-box ()
  "Init company-box
https://github.com/sebastiencs/company-box"
  (use-package company-box
    :defer t
    :hook (company-mode . company-box-mode)
    :config
    (progn
      (spacemacs|diminish company-box-mode))
    )
  )

(defun pretty-code/init-pretty-code ()
  "Init pretty code."
  (spacemacs|do-after-display-system-init
   (use-package pretty-code
     :init
     (setq +pretty-code-fira-code-font-name "Fira Code Memacs Symbol")
     :config
     (progn
       (+pretty-code|setup-fira-ligatures)
       (set-pretty-symbols! 'emacs-lisp-mode
         :lambda "lambda"
         :true "t"
         :false "nil"
         :def "defun"
         :and "and"
         :or "or"
         :not "not")
       (set-pretty-symbols! 'org-mode
         :src_block "#+BEGIN_SRC"
         :src_block_end "#+END_SRC")
       (set-pretty-symbols! 'go-mode
         :null "nil"
         :def  "func"
         :true "true"
         :false "false"
         :bool  "bool"
         :and "&&"
         :or "||"
         :not "!")
       ;; TODO define pretty-code-symbols by set-pretty-symbol! function and  `spacemacs/defer-until-after-user-config'
       )
     ))
  )

;;; packages.el ends here
