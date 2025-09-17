;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Mephis Pheies"
      user-mail-address "mephispheies@qq.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-symbol-font' -- for symbols
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Fira Code" :size 16))
(setq doom-chinese-font (font-spec :family "Xingkai SC" :size 18))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one-light)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)

;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').

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

;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
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
          :atom          "◉"
          :promise       "ℙ"
          :local         "⚲"
          :do            "❯"
          :union         "⋃"
          :intersect     "∩"
          :diff          "∖"
          :tuple         "⨂"
          :pipe          "ǀ"
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
  (sis-global-inline-mode t))

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
   :desc "align '|'" "|" #'spacemacs/align-repeat-bar)))

(when (modulep! :lang org)
  (load! "+org"))
