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
                              memacs-company-box-backend-yasnippet-selected-face)))
      )

(setq company-box-icons-unknown 'fa_question_circle)

(setq company-box-icons-elisp
      '((fa_tag        :height 1 :face font-lock-function-name-face) ;; Function
        (fa_cog        :height 1 :face font-lock-variable-name-face) ;; Variable
        (fa_cube       :height 1 :face font-lock-constant-face     ) ;; Feature
        (md_color_lens :height 1 :face font-lock-doc-face          ) ;; Face
        ))

(setq company-box-icons-yasnippet 'fa_bookmark)

(setq company-box-icons-lsp
      '((1  . (fa_text_height              :height 1                                   )) ;; Text
        (2  . (fa_tags                     :height 1 :face font-lock-function-name-face)) ;; Method
        (3  . (fa_tag                      :height 1 :face font-lock-function-name-face)) ;; Function
        (4  . (fa_tag                      :height 1 :face font-lock-function-name-face)) ;; Constructor
        (5  . (fa_cog                      :height 1 :face font-lock-builtin-face      )) ;; Field
        (6  . (fa_cog                      :height 1 :face font-lock-variable-name-face)) ;; Variable
        (7  . (fa_cube                     :height 1 :face font-lock-preprocessor-face )) ;; Class
        (8  . (fa_puzzle_piece             :height 1 :face font-lock-preprocessor-face )) ;; Interface
        (9  . (fa_cube                     :height 1 :face font-lock-preprocessor-face )) ;; Module
        (10 . (fa_cog                      :height 1 :face font-lock-builtin-face      )) ;; Property
        (11 . (md_settings_system_daydream :height 1                                   )) ;; Unit
        (12 . (fa_cog                      :height 1 :face default                     )) ;; Value
        (13 . (md_storage                  :height 1 :face font-lock-type-face         )) ;; Enum
        (14 . (md_closed_caption           :height 1 :face font-lock-keyword-face      )) ;; Keyword
        (15 . (md_closed_caption           :height 1                                   )) ;; Snippet
        (16 . (md_color_lens               :height 1 :face font-lock-doc-face          )) ;; Color
        (17 . (fa_file_text_o              :height 1                                   )) ;; File
        (18 . (md_refresh                  :height 1                                   )) ;; Reference
        (19 . (fa_folder_open              :height 1                                   )) ;; Folder
        (20 . (md_closed_caption           :height 1 :face font-lock-builtin-face      )) ;; EnumMember
        (21 . (fa_square                   :height 1 :face font-lock-constant-face     )) ;; Constant
        (22 . (fa_cube                     :height 1 :face font-lock-type-face         )) ;; Struct
        (23 . (fa_calendar                 :height 1                                   )) ;; Event
        (24 . (fa_square_o                 :height 1                                   )) ;; Operator
        (25 . (fa_arrows                   :height 1                                   )) ;; TypeParameter
        ))


;;;; all-the-icons
(setq all-the-icons-scale-factor 1)

;;; config.el ends here