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

(defvar memacs-company-box-icons-all-the-icons-org-list
  '((Unknown . (all-the-icons-faicon "cog"                                  :height 1                                   ))
    (Text . (all-the-icons-octicon "file-text"         :height 1                                   ))
    (Method . (all-the-icons-faicon "cube"                                  :height 1 :face font-lock-function-name-face))
    (Function . (all-the-icons-faicon "cube"                                :height 1 :face font-lock-function-name-face))
    (Constructor . (all-the-icons-faicon "cube"                             :height 1 :face font-lock-function-name-face))
    (Field . (all-the-icons-faicon "cog"                                    :height 1 :face font-lock-builtin-face      ))
    (Variable . (all-the-icons-faicon "cog"                                 :height 1 :face font-lock-variable-name-face))
    (Class . (all-the-icons-faicon "cogs"                                   :height 1 :face font-lock-preprocessor-face ))
    ;; (Interface . ( "Interface.png"                  :height 1 :face font-lock-preprocessor-face ))
    (Module . (all-the-icons-alltheicon "less"         :height 1 :face font-lock-preprocessor-face ))
    (Property . (all-the-icons-faicon "wrench"                              :height 1 :face font-lock-builtin-face      ))
    ;; (Unit . (all-the-icons-faicon  "Misc.png"                            :height 1                                   ))
    ;; (Value . (all-the-icons-faicon  "EnumItem.png"                       :height 1 :face default                     ))
    (Enum . (all-the-icons-material "content_copy"     :height 1 :face font-lock-type-face         ))
    ;; (Keyword . ( "Keyword.png"                      :height 1 :face font-lock-keyword-face      ))
    (Snippet . (all-the-icons-material "content_paste" :height 1                                   ))
    (Color . (all-the-icons-material "palette"         :height 1 :face font-lock-doc-face          ))
    (File . (all-the-icons-faicon "file"                                    :height 1                                   ))
    ;; (Reference . ( "Misc.png"                       :height 1                                   ))
    (Folder . (all-the-icons-faicon "folder"                                :height 1                                   ))
    ;; (EnumMember . ( "EnumItem.png"                  :height 1 :face font-lock-builtin-face      ))
    ;; (Constant . ( "Constant.png"                    :height 1 :face font-lock-constant-face     ))
    (Struct . (all-the-icons-faicon "cogs"                                  :height 1 :face font-lock-type-face         ))
    (Event . (all-the-icons-faicon "bolt"                                   :height 1                                   ))
    ;; (Operator . ( "Misc.png"                        :height 1                                   ))
    (TypeParameter . (all-the-icons-faicon "cogs"                           :height 1                                   ))
    ;; (Template . ( "Template.png"                    :height 1                                   ))
    )
  "Origin list of company-box-icons-all-the-icons,
used to be compiled to company-box-icons-all-the-icons.")

;;(Unknown . (fa_question_circle       :height 1                                   ))
;;(Text  . (fa_text_height             :height 1                                   ))
;;(Method  . (fa_tags                  :height 1 :face font-lock-function-name-face))
;;(Function  . (fa_tag                 :height 1 :face font-lock-function-name-face))
;;(Constructor  . (fa_tag              :height 1 :face font-lock-function-name-face))
;;(Field  . (fa_cog                    :height 1 :face font-lock-builtin-face      ))
;;(Variable  . (fa_cog                 :height 1 :face font-lock-variable-name-face))
;;(Class  . (fa_cube                   :height 1 :face font-lock-preprocessor-face ))
;;(Interface  . (fa_puzzle_piece       :height 1 :face font-lock-preprocessor-face ))
;;(Module  . (fa_cube                  :height 1 :face font-lock-preprocessor-face ))
;;(Property . (fa_cog                  :height 1 :face font-lock-builtin-face      ))
;;(Unit . (md_settings_system_daydream :height 1                                   ))
;;(Value . (fa_cog                     :height 1 :face default                     ))
;;(Enum . (md_storage                  :height 1 :face font-lock-type-face         ))
;;(Keyword . (md_closed_caption        :height 1 :face font-lock-keyword-face      ))
;;(Snippet . (md_closed_caption        :height 1                                   ))
;;(Color . (md_color_lens              :height 1 :face font-lock-doc-face          ))
;;(File . (fa_file_text_o              :height 1                                   ))
;;(Reference . (md_refresh             :height 1                                   ))
;;(Folder . (fa_folder_open            :height 1                                   ))
;;(EnumMember . (md_closed_caption     :height 1 :face font-lock-builtin-face      ))
;;(Constant . (fa_square               :height 1 :face font-lock-constant-face     ))
;;(Struct . (fa_cube                   :height 1 :face font-lock-type-face         ))
;;(Event . (fa_calendar                :height 1                                   ))
;;(Operator . (fa_square_o             :height 1                                   ))
;;(TypeParameter . (fa_arrows          :height 1                                   ))
;;(Template . (fa_bookmark             :height 1                                   ))


;;;; all-the-icons
(setq all-the-icons-scale-factor 1)

;;; config.el ends here
