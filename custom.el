(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel-mode t)
 '(counsel-projectile-action
   (quote
    (1
     ("o" counsel-projectile-action "current window")
     ("j" counsel-projectile-action-other-window "other window")
     ("x" counsel-projectile-action-file-extern "open file frame")
     ("r" counsel-projectile-action-file-root "open file as root"))))
 '(counsel-projectile-find-dir-action
   (quote
    (1
     ("o" counsel-projectile-find-dir-action-other-window "current window")
     ("j" counsel-projectile-find-dir-action-other-window "other window")
     ("m" counsel-projectile-find-file-action-find-file-manually "find file manually"))))
 '(counsel-projectile-find-file-action
   (quote
    (1
     ("o" counsel-projectile-find-file-action "current window")
     ("j" counsel-projectile-find-file-action-other-window "other window")
     ("x" counsel-projectile-find-file-action-extern "open frame")
     ("r" counsel-projectile-find-file-action-root "open as root"))))
 '(counsel-projectile-switch-to-buffer-action
   (quote
    (1
     ("o" counsel-projectile-switch-to-buffer-action "current window")
     ("j" switch-to-buffer-other-window "other window"))))
 '(hl-todo-keyword-faces
   (quote
    (("HOLD" . "#d0bf8f")
     ("TODO" . "#cc9393")
     ("NEXT" . "#dca3a3")
     ("THEM" . "#dc8cc3")
     ("PROG" . "#7cb8bb")
     ("OKAY" . "#7cb8bb")
     ("DONT" . "#5f7f5f")
     ("FAIL" . "#8c5353")
     ("DONE" . "#afd8af")
     ("NOTE" . "#d0bf8f")
     ("KLUDGE" . "#d0bf8f")
     ("HACK" . "#d0bf8f")
     ("FIXME" . "#cc9393")
     ("XXX" . "#cc9393")
     ("XXXX" . "#cc9393")
     ("???" . "#cc9393")
     ("READY" . "#cc9393"))))
 '(package-selected-packages
   (quote
    (mvn maven-test-mode org-category-capture pipenv spacemacs-theme ibuffer-projectile lsp-python lsp-ui ivy-xref cquery company-lsp lsp-mode centered-cursor-mode company-quickhelp smex yasnippet-snippets yapfify yaml-mode xterm-color ws-butler which-key wgrep web-mode web-beautify volatile-highlights vi-tilde-fringe uuidgen use-package toc-org tern tagedit string-inflection sql-indent spaceline smeargle slim-mode sicp shell-pop scss-mode sass-mode restart-emacs request realgud ranger rainbow-delimiters pyvenv pytest pyenv-mode py-isort pug-mode protobuf-mode popwin pippel pip-requirements persp-mode pcre2el password-generator paradox pangu-spacing ox-gfm overseer outshine orgit org-projectile org-present org-pomodoro org-download org-bullets org-brain open-junk-file nameless multi-term move-text markdown-toc macrostep lorem-ipsum livid-mode live-py-mode linum-relative link-hint leave-delimited js2-refactor js-doc ivy-rtags ivy-hydra insert-shebang indent-guide importmagic impatient-mode hy-mode hl-todo highlight-parentheses highlight-numbers highlight-indentation google-c-style goenv godoctor go-tag go-rename go-guru go-eldoc gitignore-mode gitconfig-mode gitattributes-mode git-timemachine git-messenger git-link gh-md fuzzy flycheck-rtags flycheck-pos-tip flycheck-bashate flx fish-mode fill-column-indicator eyebrowse expand-region exec-path-from-shell evil-visualstar evil-visual-mark-mode evil-unimpaired evil-surround evil-snipe evil-search-highlight-persist evil-org evil-numbers evil-nerd-commenter evil-mc evil-matchit evil-magit evil-lisp-state evil-lion evil-indent-plus evil-iedit-state evil-exchange evil-escape evil-ediff evil-cleverparens evil-args evil-anzu eval-sexp-fu eshell-z eshell-prompt-extras esh-help emmet-mode elisp-slime-nav editorconfig dumb-jump dracula-theme dockerfile-mode docker disaster diminish counsel-projectile counsel-css company-web company-statistics company-shell company-rtags company-go company-c-headers company-auctex company-anaconda column-enforce-mode clean-aindent-mode clang-format blog-admin auto-yasnippet auto-highlight-symbol auto-compile auctex-latexmk advance-words-count adaptive-wrap ace-window ace-link ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(evil-snipe-matches-face ((t :foreground "#ff5555" :weight bold))))
