;;; early-init.el --- Spacemacs Early Init File
;;
;; Copyright (c) 2018 Sylvain Benner & Contributors
;;
;; Author: Miciah Dashiel Butler Masters <miciah.masters@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; Before Emacs 27, the init file was responsible for initializing the package
;; manager by calling `package-initialize'. Emacs 27 changed the default
;; behavior: It now calls `package-initialize' before loading the init file.
;; This behavior would prevent Spacemacs's own package initialization from
;; running. However, Emacs 27 also loads the "early init" file (this file)
;; before it initializes the package manager, and Spacemacs can use this early
;; init file to prevent Emacs from initializing the package manager. (See
;; <http://git.savannah.gnu.org/cgit/emacs.git/commit/?id=24acb31c04b4048b85311d794e600ecd7ce60d3b>.)
;;
;; Earlier Emacs versions do not load the early init file and do not initialize
;; Avoid garbage collection during startup.
;; see `SPC h . dotspacemacs-gc-cons' for more info
(setq gc-cons-threshold 402653184 gc-cons-percentage 0.6)

;; the package manager before loading the init file, so this file is neither
;; needed nor loaded on those versions.
(setq package-enable-at-startup nil)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we easily halve startup times with fonts that are
;; larger than the system default.
(setq frame-inhibit-implied-resize t)

;; Ignore X resources; its settings would be redundant with the other settings
;; in this file and can conflict with later config (particularly where the
;; cursor color is concerned).
(advice-add #'x-apply-session-resources :override #'ignore)
