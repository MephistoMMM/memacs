;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Mephis Pheies"
      user-mail-address "mephispheies@qq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "Fira Code" :size 16))
(setq doom-chinese-font (font-spec :family "Kaiti SC"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)

;; Scratch buffers preserve their last major mode, however, so this only affects
;; the first, fresh scratch buffer you create. This accepts:
;;   t           Inherits the major mode of the last buffer you had selected.
;;   nil         Uses `fundamental-mode'
;;   MAJOR-MODE  Any major mode symbol"
(setq doom-scratch-initial-major-mode t)

(setq +format-on-save-disabled-modes
      '(sql-mode           ; sqlformat is currently broken
        tex-mode           ; latexindent is broken
        latex-mode
        LaTeX-mode
        org-msg-edit-mode
        emacs-lisp-mode))

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', where Emacs
;;   looks when you load packages with `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c g k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c g d') to jump to their definition and see how
;; they are implemented.

(when (and (featurep :system 'macos) (fboundp 'mac-auto-operator-composition-mode))
  (setq +ligatures-extra-symbols
        '(;; org
          :name          "»"
          :src_block     "»"
          :src_block_end "«"
          :quote         "“"
          :quote_end     "”"
          ;; Functional
          :lambda        "λ"
          :def           "ƒ"
          :composition   "∘"
          :map           "↦"
          ;; Types
          :null          "🀆"
          :true          "🌕"
          :false         "🌑"
          :int           "Ƶ"
          :float         "Ɍ"
          :str           "Ѕ"
          :bool          "🌗"
          :list          "Ꮮ"
          :empty-set     "∅"
          ;; Flow
          :not           "￢"
          :in            "∈"
          :not-in        "∉"
          :and           "∧"
          :or            "∨"
          :for           "∀"
          :while         "♺"
          :some          "∃"
          :return        "⮑"
          :yield         "⟻"
          ;; Other
          :local         "⚲"
          :do            "❯"
          :union         "⋃"
          :intersect     "∩"
          :diff          "∖"
          :tuple         "⨂"
          :pipe          "ǀ" ;; FIXME: find a non-private char
          :dot           "•")
        ))

(use-package! sis
  ;; :hook
  ;; (((text-mode prog-mode) . smart-input-source-follow-context-mode)
  ;;  ((text-mode prog-mode) . smart-input-source-inline-english-mode))
  :config
  (sis-ism-lazyman-config

   ;; English input source may be: "ABC", "US" or another one.
   ;; "com.apple.keylayout.US"
   "com.apple.keylayout.ABC"

   ;; Other language input source: "rime", "sogou" or another one.
   ;; "im.rime.inputmethod.Squirrel.Rime"
   "com.apple.inputmethod.SCIM.ITABC")

  ;; enable the /cursor color/ mode
  (sis-global-cursor-color-mode t)
  ;; enable the /respect/ mode
  (sis-global-respect-mode t)
  ;; enable the /context/ mode for all buffers
  (sis-global-context-mode t)
  ;; enable the /inline english/ mode for all buffers
  (sis-global-inline-mode t)
  )

(use-package! kana)

(use-package! image
  :custom
  ;; Enable converting external formats (ie. webp) to internal ones.
  (image-use-external-converter t))

;; add memacs keybinds group
(map!
 ;;; Leader
 (:leader
  (:prefix-map ("m" . "memacs")
   :desc "kana"    "k" #'kana)
  (:prefix-map ("ma" . "align")
   :desc "align '%'" "%" #'spacemacs/align-repeat-percent
   :desc "align '&'" "&" #'spacemacs/align-repeat-ampersand
   :desc "align '('" "(" #'spacemacs/align-repeat-left-paren
   :desc "align ')'" ")" #'spacemacs/align-repeat-right-paren
   :desc "align '{'" "{" #'spacemacs/align-repeat-left-curly-brace
   :desc "align '}'" "}" #'spacemacs/align-repeat-right-curly-brace
   :desc "align '['" "[" #'spacemacs/align-repeat-left-square-brace
   :desc "align ']'" "]" #'spacemacs/align-repeat-right-square-brace
   :desc "align ','" "," #'spacemacs/align-repeat-comma
   :desc "align '.'" "." #'spacemacs/align-repeat-decimal
   :desc "align ':'" ":" #'spacemacs/align-repeat-colon
   :desc "align ';'" ";" #'spacemacs/align-repeat-semicolon
   :desc "align '='" "=" #'spacemacs/align-repeat-equal
   :desc "align '\\'" "\\" #'spacemacs/align-repeat-backslash
   :desc "align" "a" #'align
   :desc "align-current" "c" #'align-current
   :desc "align math oper" "m" #'spacemacs/align-repeat-math-oper
   :desc "align repeat" "r" #'spacemacs/align-repeat
   :desc "align '|'" "|" #'spacemacs/align-repeat-bar))

 ;;; Kana
 (:after kana :map kana-mode-map
  :nm "v" #'kana-validate
  :nm "s" #'kana-say-question
  :nm "p" #'kana-previous
  :nm "n" #'kana-next
  :nm "t" #'kana-toggle-kana
  :nm "r" #'kana-toggle-random
  :nm "l" #'kana-loop-toggle
  :nm "]" #'kana-loop-inc
  :nm "[" #'kana-loop-dec
  :nm "a" #'kana-first
  :nm "j" #'kana-jump
  :nm "q" #'kana-quit
  :nm "d" #'kana-details)
 )

(when (modulep! :lang org)
  (load! "+org"))
