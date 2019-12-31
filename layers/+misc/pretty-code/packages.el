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
    (font-lock+ :step pre
                :location (recipe :fetcher github
                                  :repo emacsmirror/font-lock-plus))
    all-the-icons
    all-the-icons-ivy

    (company-box :requires company)
    unicode-fonts
    ))

(defun pretty-code/init-all-the-icons ()
  "Init all the icons.
 run `M-x all-the-icons-install-fonts' automatically"
  (use-package all-the-icons
    :if (display-graphic-p)
    :after company
    :init
    (unless (member "all-the-icons" (font-family-list))
      (all-the-icons-install-fonts t))
    :config
    (progn
      (require 'cl)
      (cl-loop
       for item in memacs-all-the-icons-alist
       do  (add-to-list (car item) (cdr item)))
      ;; is linux, there is a bug in linux
      ;;   do noting
      ;; is mac
      (unless (spacemacs/system-is-linux)
        (setq company-box-icons-all-the-icons
              (cl-loop
               for aicon in memacs-company-box-icons-all-the-icons-org-list
               collect (cons
                        (car aicon)
                        (apply (car (cdr aicon)) (cdr (cdr aicon)))))))
      )
    )
  )

(defun pretty-code/init-all-the-icons-ivy ()
  "Init all the icons for ivy. "
  (use-package all-the-icons-ivy
    :if (configuration-layer/package-used-p 'all-the-icons)
    :init
    (progn
      (add-hook 'spacemacs-post-user-config-hook
                #'all-the-icons-ivy-setup)
      (with-eval-after-load 'counsel-projectile
        (all-the-icons-ivy-setup)))
    )
  )

(defun pretty-code/init-font-lock+()
  "Init font-lock+.")

(defun pretty-code/init-unicode-fonts()
  "Init unicode-fonts."
  (setq-default bidi-display-reordering t)
  (add-hook 'after-make-frame-functions #'pretty-code//unicode-setup-fonts-h)
  )

(defun pretty-code/init-company-box ()
  "Init company-box
https://github.com/sebastiencs/company-box"
  (use-package company-box
    :defer t
    :hook '(company-mode . company-box-mode)
    :commands 'company-box-doc-manually
    :init (setq company-box-icons-alist 'company-box-icons-all-the-icons)
    :config
    (progn
      (spacemacs|hide-lighter company-box-mode)
      (setq company-box-backends-colors nil
            company-box-max-candidates 1000
            company-box-doc-enable t)
      (add-hook 'company-box-selection-hook
                (lambda (selection frame) (company-box-doc--hide frame))))
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
