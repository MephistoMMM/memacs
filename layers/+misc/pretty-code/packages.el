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
    :init
    (progn
      (setq company-box-icons-unknown 'fa_question_circle)

      (setq company-box-icons--elisp
            '((fa_tag :face font-lock-function-name-face) ;; Function
              (fa_cog :face font-lock-variable-name-face) ;; Variable
              (fa_cube :face font-lock-constant-face) ;; Feature
              (md_color_lens :face font-lock-doc-face))) ;; Face

      (setq company-box-icons--yasnippet 'fa_bookmark)

      (setq company-box-icons--lsp
            '((1 . fa_text_height) ;; Text
              (2 . (fa_tags :face font-lock-function-name-face)) ;; Method
              (3 . (fa_tag :face font-lock-function-name-face)) ;; Function
              (4 . (fa_tag :face font-lock-function-name-face)) ;; Constructor
              (5 . (fa_cog :foreground "#FF9800")) ;; Field
              (6 . (fa_cog :foreground "#FF9800")) ;; Variable
              (7 . (fa_cube :foreground "#7C4DFF")) ;; Class
              (8 . (fa_cube :foreground "#7C4DFF")) ;; Interface
              (9 . (fa_cube :foreground "#7C4DFF")) ;; Module
              (10 . (fa_cog :foreground "#FF9800")) ;; Property
              (11 . md_settings_system_daydream) ;; Unit
              (12 . (fa_cog :foreground "#FF9800")) ;; Value
              (13 . (md_storage :face font-lock-type-face)) ;; Enum
              (14 . (md_closed_caption :foreground "#009688")) ;; Keyword
              (15 . md_closed_caption) ;; Snippet
              (16 . (md_color_lens :face font-lock-doc-face)) ;; Color
              (17 . fa_file_text_o) ;; File
              (18 . md_refresh) ;; Reference
              (19 . fa_folder_open) ;; Folder
              (20 . (md_closed_caption :foreground "#009688")) ;; EnumMember
              (21 . (fa_square :face font-lock-constant-face)) ;; Constant
              (22 . (fa_cube :face font-lock-type-face)) ;; Struct
              (23 . fa_calendar) ;; Event
              (24 . fa_square_o) ;; Operator
              (25 . fa_arrows)) ;; TypeParameter
            )
      ))
  )

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
