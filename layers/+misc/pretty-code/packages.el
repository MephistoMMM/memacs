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
        ))

(defun pretty-code/init-pretty-code ()
  "Init pretty code."
  (spacemacs|do-after-display-system-init
    (use-package pretty-code
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
