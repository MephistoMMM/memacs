;;; config.el --- macOS Layer config File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(when (spacemacs/system-is-mac)
  (defvar osx-command-as 'meta
    "Sets the key binding of the `COMMAND' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `none'.
   Default: `hyper'.")
  ;; There are problems setting osx-command-as to `alt' and `super',
  ;; so we use `hyper' as a default instead because, for example:
  ;;   - Using `alt':   Command-x or Command-m inserts, respectively: × µ
  ;;   - Using `super': Control-Command-f produces keycode: <C-s-268632078>
  ;; Setting to `hyper' seems to avoid both types of the above problems.
  ;; Also, while it is possible, it is not recommended to set to `meta'
  ;; since standard macOS shortcuts would overshadow important keys such
  ;; as M-x.

  (defvar osx-option-as 'super
    "Sets the key binding of the `OPTION' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `none'.
   Default: `meta'.
   For backwards compatibility the variable `osx-use-option-as-meta'
   takes precedence is set to t.")
  (defvar osx-function-as nil
    "Sets the key binding of the `FUNCTION' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `nil'.
   Default: `nil'.")
  (defvar osx-control-as 'control
    "Sets the key binding of the `CONTROL' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `none'.
   Default: `control'.")

  (defvar osx-right-control-as 'left
    "Sets the key binding of the `RIGHT CONTROL' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")
  (defvar osx-right-command-as 'left
    "Sets the key binding of the `RIGHT COMMAND' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")
  (defvar osx-right-option-as 'left
    "Sets the key binding of the `RIGHT OPTION' key on macOS.
   Possible values are `super' `meta' `hyper' `alt' `left' `none'.
   Default: `left'.")

  (defvar memacs-autoescape-english-layout-name "ABC"
    "English layout name in your macOS system")

  (defvar memacs-iterm2-pyenv-python-interpreter
    (expand-file-name "~/Library/ApplicationSupport/iTerm2/iterm2env/versions/3.7.2/bin/python")
    "Path of python interpreter in iTerm2 pyenv")

  (defvar memacs-iterm2-python-fallback-interpreter
    "/usr/local/bin/python"
    "Path of fallback python interpreter if iTerm2 pyenv not exsiting")

  (defvar osx-use-dictionary-app t
    "Use the macOS dictionary app instead of Wordnet.")

  (defvar memacs-iterm2-scripts-path
    (expand-file-name (concat user-emacs-directory "local/mpiterm2/"))
    "Path of directory stores scripts about iterm2")
  )

;; Use the macOS Emoji font for Emoticons.
(when (fboundp 'set-fontset-font)
  (set-fontset-font "fontset-default"
                    '(#x1F600 . #x1F64F)
                    (font-spec :name "Apple Color Emoji") nil 'prepend))
