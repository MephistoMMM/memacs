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
  '((Unknown . (all-the-icons-material "find_in_page" :height 0.85 :v-adjust -0.2))
    (Text . (all-the-icons-faicon "text-width" :height 0.8 :v-adjust -0.05))
    (Method . (all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face all-the-icons-purple))
    (Function . (all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face all-the-icons-purple))
    (Constructor . (all-the-icons-faicon "cube" :height 0.8 :v-adjust -0.05 :face all-the-icons-purple))
    (Field . (all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face all-the-icons-lblue))
    (Variable . (all-the-icons-octicon "tag" :height 0.8 :v-adjust 0 :face all-the-icons-lblue))
    (Class . (all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face all-the-icons-orange))
    (Interface . (all-the-icons-material "share" :height 0.85 :v-adjust -0.2 :face all-the-icons-lblue))
    (Module . (all-the-icons-material "view_module" :height 0.85 :v-adjust -0.2 :face all-the-icons-lblue))
    (Property . (all-the-icons-faicon "wrench" :height 0.8 :v-adjust -0.05))
    (Unit . (all-the-icons-material "settings_system_daydream" :height 0.85 :v-adjust -0.2))
    (Value . (all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face all-the-icons-lblue))
    (Enum . (all-the-icons-material "storage" :height 0.85 :v-adjust -0.2 :face all-the-icons-orange))
    (Keyword . (all-the-icons-material "filter_center_focus" :height 0.85 :v-adjust -0.2))
    (Snippet . (all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2))
    (Color . (all-the-icons-material "palette" :height 0.85 :v-adjust -0.2))
    (File . (all-the-icons-faicon "file-o" :height 0.85 :v-adjust -0.05))
    (Reference . (all-the-icons-material "collections_bookmark" :height 0.85 :v-adjust -0.2))
    (Folder . (all-the-icons-faicon "folder-open" :height 0.85 :v-adjust -0.05))
    (EnumMember . (all-the-icons-material "format_align_right" :height 0.85 :v-adjust -0.2 :face all-the-icons-lblue))
    (Constant . (all-the-icons-faicon "square-o" :height 0.85 :v-adjust -0.05))
    (Struct . (all-the-icons-material "settings_input_component" :height 0.85 :v-adjust -0.2 :face all-the-icons-orange))
    (Event . (all-the-icons-faicon "bolt" :height 0.8 :v-adjust -0.05 :face all-the-icons-orange))
    (Operator . (all-the-icons-material "control_point" :height 0.85 :v-adjust -0.2))
    (TypeParameter . (all-the-icons-faicon "arrows" :height 0.8 :v-adjust -0.05))
    (Template . (all-the-icons-material "format_align_center" :height 0.85 :v-adjust -0.2)))
  "Origin list of company-box-icons-all-the-icons,
used to be compiled to company-box-icons-all-the-icons.")

(defvar memacs-all-the-icons-alist
  '(
    (all-the-icons-mode-icon-alist . (vterm-mode all-the-icons-octicon "terminal" :v-adjust 0.2))
    (all-the-icons-icon-alist . ("\\.xpm$" all-the-icons-octicon "file-media" :v-adjust 0.0 :face all-the-icons-dgreen))
    (all-the-icons-icon-alist . ("\\.toml$" all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
    (all-the-icons-mode-icon-alist . (conf-toml-mode all-the-icons-octicon "settings" :v-adjust 0.0 :face all-the-icons-dyellow))
    (all-the-icons-icon-alist . ("\\.lua$" all-the-icons-fileicon "lua" :face all-the-icons-dblue))
    (all-the-icons-mode-icon-alist . (lua-mode all-the-icons-fileicon "lua" :face all-the-icons-dblue))
    (all-the-icons-mode-icon-alist . (help-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1 :face all-the-icons-purple))
    (all-the-icons-mode-icon-alist . (Info-mode all-the-icons-faicon "info-circle" :height 1.1 :v-adjust -0.1))
    (all-the-icons-icon-alist . ("NEWS$" all-the-icons-faicon "newspaper-o" :height 0.9 :v-adjust -0.2))
    (all-the-icons-icon-alist . ("Cask\\'" all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
    (all-the-icons-mode-icon-alist . (cask-mode all-the-icons-fileicon "elisp" :height 1.0 :v-adjust -0.2 :face all-the-icons-blue))
    (all-the-icons-icon-alist . (".*\\.ipynb\\'" all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
    (all-the-icons-mode-icon-alist . (ein:notebooklist-mode all-the-icons-faicon "book" :face all-the-icons-orange))
    (all-the-icons-mode-icon-alist . (ein:notebook-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
    (all-the-icons-mode-icon-alist . (ein:notebook-multilang-mode all-the-icons-fileicon "jupyter" :height 1.2 :face all-the-icons-orange))
    (all-the-icons-icon-alist . ("\\.epub\\'" all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
    (all-the-icons-mode-icon-alist . (nov-mode all-the-icons-faicon "book" :height 1.0 :v-adjust -0.1 :face all-the-icons-green))
    (all-the-icons-mode-icon-alist . (gfm-mode all-the-icons-octicon "markdown" :face all-the-icons-lblue))
    )
  "Origin list of custom all-the-icons-mode-icon-alist or all-the-icons-alist,
used to be compiled to company-box-icons-all-the-icons."
  )

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
