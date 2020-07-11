;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; refresh' after modifying this file!


;; These are used for a number of things, particularly for GPG configuration,
;; some email clients, file templates and snippets.
(setq user-full-name "Mephis Pheies"
      user-mail-address "mephistommm@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
(setq doom-font (font-spec :family "monofur for Powerline" :size 18))
(setq doom-chinese-font (font-spec :family "Kaiti SC"))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. These are the defaults.
(setq doom-theme 'doom-one)

;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/Dropbox/org"
      +org-capture-work-directory "~/Documents/works"
      org-attach-id-dir (expand-file-name "~/Dropbox/org/statics/")
      org-download-image-dir (expand-file-name "~/Dropbox/org/statics/")
      org-roam-db-location (expand-file-name "~/.local/roam/org-roam.db"))

;; If you want to change the style of line numbers, change this to `relative' or
;; `nil' to disable it:
(setq display-line-numbers-type 'relative)

;; Scratch buffers preserve their last major mode, however, so this only affects
;; the first, fresh scratch buffer you create. This accepts:
;;   t           Inherits the major mode of the last buffer you had selected.
;;   nil         Uses `fundamental-mode'
;;   MAJOR-MODE  Any major mode symbol"
(setq doom-scratch-initial-major-mode t)


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

;; Migrate from `modules/lang/org'
(use-package! org-fancy-priorities ; priority icons
  :hook (org-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("☢" "☕" "■")))
;; ("❗" "⬆" "⬇" "☕")

;; Missions
(setq memacs-mission-starter-mission-list
      '(
        ;; config new org file in dropbox
        ("Dropbox Note Buffer" org-mode
          (concat org-directory "/notes/") (memacs//mission-start-find-file-name nil))
        ("Codewar&Golang" go-mode
          (format-time-string "~/Workspace/go/src/codewar_pg/%Y_%m_%d"))
        ;; ("Daily Report" org-mode
        ;;   "~/Documents/works/seven/daily_report" (format-time-string "%Y_%m_%d.org"))
        ))

;; Helps
(setq memacs-mission-helper-help-list
      '(
        ("Projectile"
          ("ignoring files" "https://projectile.readthedocs.io/en/latest/projects/#ignoring-files"))
        ("Dockerfile"
          ("docker reference" "https://docs.docker.com/engine/reference/builder/#usage")
          ("compose reference" "https://docs.docker.com/compose/compose-file/"))
        ("Golang"
          ("debugger: Delve" "https://github.com/derekparker/delve/blob/master/Documentation/cli/README.md"))
        ("Tools"
          ("RestClient" "https://github.com/pashky/restclient.el"))
        ("Applescript" "http://downloads.techbarrack.com/books/programming/AppleScript/website/index.html")
        )
      )

(use-package! kana)

;; add memacs keybinds group
(map!
 :leader
 (:prefix-map ("m" . "memacs")
   :desc "starter" "s" #'+memacs/mission-starter-start
   :desc "helper"  "h" #'+memacs/mission-helper-help
   :desc "kana"    "k" #'kana)
 )

(map!
 ;; kana
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

(when (featurep! :lang org)
  (load! "+org"))
