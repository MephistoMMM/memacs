;;; purifier-theme.el --- Purifier Theme based on ayu-theme

;; Copyright 2019-present, All rights reserved
;;
;; Code licensed under the MIT license

;; Author: Mephis Pheies

;;; Commentary:

;; A dark color theme available for a number of editors.

;;; Code:

(deftheme purifier)

(let ((class '((class color) (min-colors 89)))
      (fg1 "#e2e2dc")  ;; fg
      (fg2 "#FAFAFA")
      (fg3 "#CBCCC6")
      (fg4 "#6C7680")
      (bg1 "#0A0E14")  ;; bg
      (bg2 "#1F2430")
      (bg3 "#3D424D")
      (bg4 "#464752")
      (bg5 "#1F2430")
      (builtin "#73D0FF")
      (keyword "#FF8F40") ;; keywords
      (const   "#FFEE99") ;; const
      (comment "#626A73") ;; comment
      (func    "#FFB454") ;; func
      (str     "#C2D94C") ;; string
      (tag     "#39BAE6") ;; tag
      (entity  "#59C2FF") ;; entity
      (interface "#A37ACC")
      (type    "#39BAE6")
      (var     "#59C2FF")
      (markup "#F29668")   ;; markup
      ;; (operator "#F29668") ;; operator
      (regexp  "#95E6CB") ;; regexp
      (warning "#F27983")
      (err     "#FF3333") ;; error
      (rainbow-1 "#f8f8f2")
      (rainbow-2 "#8be9fd")
      (rainbow-3 "#bd93f9")
      (rainbow-4 "#ff79c6")
      (rainbow-5 "#ffb86c")
      (rainbow-6 "#50fa7b")
      (rainbow-7 "#f1fa8c")
      (rainbow-8 "#0189cc")
      (rainbow-9 "#ff5555")
      (head1 "#55B4D4")
      (head2 "#FFB454")
      (head3 "#BAE67E")
      (head4 "#f1fa8c")
      (eph-verbatim "#f1fa8c")
      (eph-code "#ff79c6")

      ;; self defines
      (black "#121212")
      )

  (custom-theme-set-faces
   'purifier
   ;; default
   `(cursor ((,class (:background ,const))))
   `(default ((((type nil)) (:background "#000000" :foreground ,fg1))
              (,class (:background ,bg1 :foreground ,fg1))))
   `(default-italic ((,class (:italic t))))
   `(ffap ((,class (:foreground ,fg4))))
   `(fringe ((,class (:background ,bg1 :foreground ,fg4))))
   `(highlight ((,class (:foreground ,fg2 :background ,bg3))))
   `(hl-line ((,class (:background  ,bg5))))
   `(info-quoted-name ((,class (:foreground ,builtin))))
   `(info-string ((,class (:foreground ,str))))
   `(lazy-highlight ((,class (:foreground ,fg2 :background ,markup))))
   `(link ((,class (:foreground ,const :underline t))))
   `(linum ((,class (:slant italic :foreground ,bg4 :background ,bg1))))
   `(minibuffer-prompt ((,class (:bold t :foreground ,keyword))))
   `(region ((,class (:background ,type :foreground ,bg1))))
   `(show-paren-match-face ((,class (:background ,warning))))
   `(trailing-whitespace ((,class :foreground nil :background ,warning)))
   `(vertical-border ((,class (:foreground ,bg3))))
   `(warning ((,class (:foreground ,warning))))
   `(whitespace-trailing ((,class :inherit trailing-whitespace)))
   ;; syntax
   `(font-lock-builtin-face ((,class (:foreground ,builtin))))
   `(font-lock-comment-face ((,class (:foreground ,comment))))
   `(font-lock-constant-face ((,class (:foreground ,const))))
   `(font-lock-doc-face ((,class (:foreground ,comment))))
   `(font-lock-function-name-face ((,class (:foreground ,func :bold t))))
   `(font-lock-keyword-face ((,class (:bold ,class :foreground ,keyword))))
   `(font-lock-negation-char-face ((,class (:foreground ,const))))
   `(font-lock-reference-face ((,class (:foreground ,const))))
   `(font-lock-string-face ((,class (:foreground ,str))))
   `(font-lock-type-face ((,class (:foreground ,type ))))
   `(font-lock-variable-name-face ((,class (:foreground ,var))))
   `(font-lock-warning-face ((,class (:foreground ,warning :background ,bg2))))
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,regexp))))
   `(font-lock-preprocessor-face ((,class (:foreground ,interface))))
   ;; auto-complete
   `(ac-completion-face ((,class (:underline t :foreground ,keyword))))
   ;; company
   `(company-echo-common ((,class (:foreground ,bg1 :background ,fg1))))
   `(company-preview ((,class (:background ,bg1 :foreground ,tag))))
   `(company-preview-common ((,class (:foreground ,bg2 :foreground ,fg3))))
   `(company-preview-search ((,class (:foreground ,type :background ,bg1))))
   `(company-scrollbar-bg ((,class (:background ,bg3))))
   `(company-scrollbar-fg ((,class (:foreground ,keyword))))
   `(company-template-field ((,class (:inherit region))))
   `(company-tooltip ((,class (:foreground ,fg3 :background ,bg2))))
   `(company-tooltip-annotation ((,class (:foreground ,comment))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,bg1 :bold t))))
   `(company-tooltip-common ((,class ( :foreground ,fg3))))
   `(company-tooltip-common-selection ((,class (:foreground ,builtin))))
   `(company-tooltip-mouse ((,class (:inherit highlight))))
   `(company-tooltip-selection ((,class (:background ,var :foreground ,bg1))))
   `(company-tooltip-search ((,class (:foreground ,markup :weight bold))))
   `(company-tooltip-search-selection ((,class (:foreground ,markup :weight bold))))
   ;; company-box
   `(company-box-candidate ((,class (:inherit company-tooltip))))
   `(company-box-annotation ((,class (:inherit company-tooltip-annotation))))
   `(company-box-selection ((,class (:inherit company-tooltip-selection))))
   `(company-box-background ((,class (:inherit company-tooltip))))
   `(company-box-scrollbar ((,class (:inherit company-box-selection))))
   `(memacs-company-box-backend-yasnippet-face ((,class (:foreground ,str :background ,bg2))))
   `(memacs-company-box-backend-yasnippet-selected-face ((,class (:foreground ,bg2 :background ,str))))
   ;; lsp ui
   `(lsp-ui-sideline-symbol ((,class (:foreground ,comment :box (:line-width -1 :color ,comment) :height 0.99))))
   `(lsp-ui-sideline-current-symbol ((,class (:foreground ,const :weight ultra-bold :box (:line-width -1 :color ,const) :height 0.99))))
   `(lsp-ui-sideline-code-action ((,class (:foreground ,str))))
   `(lsp-ui-sideline-symbol-info ((,class  (:slant italic :height 0.99))))
   `(lsp-ui-doc-background ((,class  (:foreground ,fg1  :background ,bg1))))
   `(lsp-ui-doc-header ((,class  (:foreground "gold"  :background ,bg1))))
   `(lsp-ui-doc-url ((,class  (:inherit link))))
   ;; diff-hl
   `(diff-hl-change ((,class (:foreground ,rainbow-5 :background ,rainbow-5))))
   `(diff-hl-delete ((,class (:foreground ,rainbow-9 :background ,rainbow-9))))
   `(diff-hl-insert ((,class (:foreground ,rainbow-6 :background ,rainbow-6))))
   ;; icomplete
   `(icompletep-determined ((,class :foreground ,builtin)))
   ;; ido
   `(ido-first-match ((,class (:foreground ,keyword :bold t))))
   `(ido-only-match ((,class (:foreground ,warning))))
   `(ido-subdir ((,class (:foreground ,builtin))))
   ;; isearch
   `(isearch ((,class (:bold t :foreground ,markup :background ,bg3))))
   `(isearch-fail ((,class (:foreground ,bg1 :background ,warning))))
   ;; js2-mode
   `(js2-external-variable ((,class (:foreground ,type  ))))
   `(js2-function-param ((,class (:foreground ,const))))
   `(js2-jsdoc-html-tag-delimiter ((,class (:foreground ,str))))
   `(js2-jsdoc-html-tag-name ((,class (:foreground ,tag))))
   `(js2-jsdoc-value ((,class (:foreground ,str))))
   `(js2-private-function-call ((,class (:foreground ,const))))
   `(js2-private-member ((,class (:foreground ,fg3))))
   ;; js3-mode
   `(js3-error-face ((,class (:underline ,warning))))
   `(js3-external-variable-face ((,class (:foreground ,var))))
   `(js3-function-param-face ((,class (:foreground ,markup))))
   `(js3-instance-member-face ((,class (:foreground ,const))))
   `(js3-jsdoc-tag-face ((,class (:foreground ,keyword))))
   `(js3-warning-face ((,class (:underline ,keyword))))
   ;; magit
   `(magit-branch ((,class (:foreground ,const :weight bold))))
   `(magit-diff-context-highlight ((,class (:background ,bg3 :foreground ,fg3))))
   `(magit-diff-file-header ((,class (:foreground ,fg2 :background ,bg3))))
   `(magit-diffstat-added   ((,class (:foreground ,type))))
   `(magit-diffstat-removed ((,class (:foreground ,var))))
   `(magit-hash ((,class (:foreground ,fg2))))
   `(magit-hunk-heading           ((,class (:background ,bg3))))
   `(magit-hunk-heading-highlight ((,class (:background ,bg3))))
   `(magit-item-highlight ((,class :background ,bg3)))
   `(magit-log-author ((,class (:foreground ,fg3))))
   `(magit-process-ng ((,class (:foreground ,warning :weight bold))))
   `(magit-process-ok ((,class (:foreground ,func :weight bold))))
   `(magit-section-heading        ((,class (:foreground ,keyword :weight bold))))
   `(magit-section-highlight      ((,class (:background ,bg2))))
   ;; mode-line
   `(mode-line ((,class (:foreground nil :background ,bg1 :box ,bg1))))
   `(mode-line-buffer-id ((,class (:inherit bold :foreground ,const))))
   `(mode-line-inactive ((,class (:foreground ,comment :background ,bg1 :box ,bg1))))
   ;; org
   `(org-agenda-date ((,class (:foreground ,rainbow-2 :underline nil))))
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))))
   `(org-agenda-done ((,class (:foreground ,rainbow-6))))
   `(org-agenda-structure ((,class (:foreground ,rainbow-3))))
   `(org-block ((,class (:foreground ,rainbow-5))))
   `(org-code ((,class (:foreground ,rainbow-7))))
   `(org-column ((,class (:background ,bg4))))
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))))
   `(org-date ((,class (:foreground ,rainbow-2 :underline t))))
   `(org-document-info ((,class (:foreground ,rainbow-8))))
   `(org-document-info-keyword ((,class (:foreground ,comment))))
   `(org-document-title ((,class (:weight bold :foreground ,rainbow-5 :height 1.44))))
   `(org-done ((,class (:foreground ,rainbow-6))))
   `(org-ellipsis ((,class (:foreground ,comment))))
   `(org-footnote ((,class (:foreground ,rainbow-8))))
   `(org-formula ((,class (:foreground ,rainbow-4))))
   `(org-headline-done ((,class (:foreground ,comment :bold nil :strike-through t))))
   `(org-hide ((,class (:foreground ,bg1 :background ,bg1))))
   `(org-level-1 ((,class (:inherit bold :foreground ,head1 :height 1.3))))
   `(org-level-2 ((,class (:inherit bold :foreground ,head2 :height 1.1))))
   `(org-level-3 ((,class (:bold nil :foreground ,head3 :height 1.0))))
   `(org-level-4 ((,class (:bold nil :foreground ,head4))))
   `(org-level-5 ((,class (:bold nil :foreground ,head1))))
   `(org-level-6 ((,class (:bold nil :foreground ,head2))))
   `(org-level-7 ((,class (:bold nil :foreground ,head3))))
   `(org-level-8 ((,class (:bold nil :foreground ,head4))))
   `(org-link ((,class (:foreground ,rainbow-2 :underline t))))
   `(org-priority ((,class (:foreground ,rainbow-2))))
   `(org-scheduled ((,class (:foreground ,rainbow-6))))
   `(org-scheduled-previously ((,class (:foreground ,rainbow-7))))
   `(org-scheduled-today ((,class (:foreground ,rainbow-6))))
   `(org-sexp-date ((,class (:foreground ,fg4))))
   `(org-special-keyword ((,class (:foreground ,rainbow-7))))
   `(org-table ((,class (:foreground ,rainbow-3))))
   `(org-tag ((,class (:foreground ,rainbow-4 :bold t :background ,bg2))))
   `(org-todo ((,class (:foreground ,rainbow-5 :bold t :background ,bg2))))
   `(org-upcoming-deadline ((,class (:foreground ,rainbow-7))))
   `(org-warning ((,class (:weight bold :foreground ,rainbow-4))))
   ;; outline
   `(outline-1 ((,class (:foreground ,rainbow-6))))
   `(outline-2 ((,class (:foreground ,rainbow-3))))
   `(outline-3 ((,class (:foreground ,rainbow-2))))
   `(outline-4 ((,class (:foreground ,rainbow-5))))
   `(outline-5 ((,class (:foreground ,rainbow-5))))
   `(outline-6 ((,class (:foreground ,rainbow-8))))
   ;; flycheck-posframe
   `(flycheck-posframe-warning-face ((,class (:foreground ,bg1 :background ,str))))
   `(flycheck-posframe-error-face ((,class (:foreground ,bg1 :background ,err))))
   ;; powerline
   `(powerline-evil-base-face ((t (:foreground ,bg2))))
   `(powerline-evil-emacs-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-7))))
   `(powerline-evil-insert-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-2))))
   `(powerline-evil-motion-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-3))))
   `(powerline-evil-normal-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-6))))
   `(powerline-evil-operator-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-4))))
   `(powerline-evil-replace-face ((,class (:inherit powerline-evil-base-face :background ,err))))
   `(powerline-evil-visual-face ((,class (:inherit powerline-evil-base-face :background ,rainbow-5))))
   ;; evil snipe
   `(evil-snipe-matches-face ((,class (:foreground ,err :weight bold))))
   ;; rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class :foreground ,rainbow-1)))
   `(rainbow-delimiters-depth-2-face ((,class :foreground ,rainbow-2)))
   `(rainbow-delimiters-depth-3-face ((,class :foreground ,rainbow-3)))
   `(rainbow-delimiters-depth-4-face ((,class :foreground ,rainbow-4)))
   `(rainbow-delimiters-depth-5-face ((,class :foreground ,rainbow-5)))
   `(rainbow-delimiters-depth-6-face ((,class :foreground ,rainbow-6)))
   `(rainbow-delimiters-depth-7-face ((,class :foreground ,rainbow-7)))
   `(rainbow-delimiters-depth-8-face ((,class :foreground ,rainbow-8)))
   `(rainbow-delimiters-unmatched-face ((,class :foreground ,warning)))
   ;; slime
   `(slime-repl-inputed-output-face ((,class (:foreground ,type))))
   ;; spam
   `(spam ((,class (:inherit gnus-summary-normal-read :foreground ,warning :strike-through t :slant oblique))))
   ;; web-mode
   `(web-mode-builtin-face ((,class (:inherit ,font-lock-builtin-face))))
   `(web-mode-comment-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-constant-face ((,class (:inherit ,font-lock-constant-face))))
   `(web-mode-doctype-face ((,class (:inherit ,font-lock-comment-face))))
   `(web-mode-function-name-face ((,class (:inherit ,font-lock-function-name-face))))
   `(web-mode-html-attr-name-face ((,class (:foreground ,type))))
   `(web-mode-html-attr-value-face ((,class (:foreground ,func))))
   `(web-mode-html-tag-face ((,class (:foreground ,keyword :bold t))))
   `(web-mode-keyword-face ((,class (:foreground ,keyword))))
   `(web-mode-string-face ((,class (:foreground ,str))))
   `(web-mode-type-face ((,class (:inherit ,font-lock-type-face))))
   `(web-mode-warning-face ((,class (:inherit ,font-lock-warning-face))))
   ;; which-func
   `(which-func ((,class (:inherit ,font-lock-function-name-face))))
   `(dired-directory ((,class (:foreground ,func :weight normal))))
   `(dired-flagged ((,class (:foreground ,keyword))))
   `(dired-header ((,class (:foreground ,fg3 :background ,bg1))))
   `(dired-ignored ((,class (:inherit shadow))))
   `(dired-mark ((,class (:foreground ,var :weight bold))))
   `(dired-marked ((,class (:foreground ,builtin :weight bold))))
   `(dired-perm-write ((,class (:foreground ,fg3 :underline t))))
   `(dired-symlink ((,class (:foreground ,str :weight normal :slant italic))))
   `(dired-warning ((,class (:foreground ,warning :underline t))))
   `(diredp-compressed-file-name ((,class (:foreground ,fg3))))
   `(diredp-compressed-file-suffix ((,class (:foreground ,fg4))))
   `(diredp-date-time ((,class (:foreground ,var))))
   `(diredp-deletion-file-name ((,class (:foreground ,keyword :background ,bg5))))
   `(diredp-deletion ((,class (:foreground ,keyword :weight bold))))
   `(diredp-dir-heading ((,class (:foreground ,fg2 :background ,bg4))))
   `(diredp-dir-name ((,class (:inherit dired-directory))))
   `(diredp-dir-priv ((,class (:inherit dired-directory))))
   `(diredp-executable-tag ((,class (:foreground ,builtin))))
   `(diredp-file-name ((,class (:foreground ,fg1))))
   `(diredp-file-suffix ((,class (:foreground ,fg4))))
   `(diredp-flag-mark-line ((,class (:foreground ,fg2 :slant italic :background ,bg5))))
   `(diredp-flag-mark ((,class (:foreground ,fg2 :weight bold :background ,bg5))))
   `(diredp-ignored-file-name ((,class (:foreground ,fg1))))
   `(diredp-mode-line-flagged ((,class (:foreground ,warning))))
   `(diredp-mode-line-marked ((,class (:foreground ,warning))))
   `(diredp-no-priv ((,class (:foreground ,fg1))))
   `(diredp-number ((,class (:foreground ,const))))
   `(diredp-other-priv ((,class (:foreground ,builtin))))
   `(diredp-rare-priv ((,class (:foreground ,builtin))))
   `(diredp-read-priv ((,class (:foreground ,type))))
   `(diredp-write-priv ((,class (:foreground ,keyword))))
   `(diredp-exec-priv ((,class (:foreground ,str))))
   `(diredp-symlink ((,class (:foreground ,warning))))
   `(diredp-link-priv ((,class (:foreground ,warning))))
   `(diredp-autofile-name ((,class (:foreground ,str))))
   `(diredp-tagged-autofile-name ((,class (:foreground ,str))))

;;;;; ahs
   `(ahs-face ((,class (:background ,bg3))))
   `(ahs-plugin-whole-buffer-face ((,class (:background ,rainbow-4 :foreground ,bg1))))

;;;;; anzu-mode
   `(anzu-mode-line ((,class (:foreground ,rainbow-7 :inherit bold))))

;;;;; eldoc
   `(eldoc-highlight-function-argument ((,class (:foreground ,rainbow-4 :inherit bold))))

;;;;; evil
   `(evil-ex-substitute-matches ((,class (:background ,markup :foreground ,fg2))))
   `(evil-ex-substitute-replacement ((,class (:background ,bg1 :foreground ,regexp))))

;;;;; flycheck
   `(flycheck-error
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,err)))
      (,class (:foreground ,builtin :background ,err :inherit bold :underline t))))
   `(flycheck-error-list-checker-name ((,class (:foreground ,keyword))))
   `(flycheck-fringe-error ((,class (:foreground ,err :inherit bold))))
   `(flycheck-fringe-info ((,class (:foreground ,keyword :inherit bold))))
   `(flycheck-fringe-warning ((,class (:foreground ,warning :inherit bold))))
   `(flycheck-info
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,keyword)))
      (,class (:foreground ,builtin :background ,keyword :inherit bold :underline t))))
   `(flycheck-warning
     ((,(append '((supports :underline (:style line))) class)
       (:underline (:style line :color ,warning)))
      (,class (:foreground ,builtin :background ,warning :inherit bold :underline t))))

;;;;; flymake
   `(flymake-error ((,(append '((supports :underline (:style line))) class)
                     (:underline (:style line :color ,err)))
                    (,class (:foreground ,builtin :background ,err :inherit bold :underline t))))
   `(flymake-note ((,(append '((supports :underline (:style line))) class)
                    (:underline (:style wave :color ,keyword)))
                   (,class (:foreground ,builtin :background ,keyword :inherit bold :underline t))))
   `(flymake-warning ((,(append '((supports :underline (:style line))) class)
                       (:underline (:style line :color ,warning)))
                      (,class (:foreground ,builtin :background ,warning :inherit bold :underline t))))

;;;;; flyspell
   `(flyspell-incorrect ((,(append '((supports :underline (:style line))) class)
                          (:underline (:style wave :color ,warning)))
                         (,class (:foreground ,builtin :background ,warning :inherit bold :underline t))))
   `(flyspell-duplicate ((,(append '((supports :underline (:style line))) class)
                          (:underline (:style wave :color ,keyword)))
                         (,class (:foreground ,builtin :background ,keyword :inherit bold :underline t))))

;;;;; git-gutter-fr
   `(git-gutter-fr:added ((,class (:foreground ,rainbow-6 :inherit bold))))
   `(git-gutter-fr:deleted ((,class (:foreground ,warning :inherit bold))))
   `(git-gutter-fr:modified ((,class (:foreground ,keyword :inherit bold))))

;;;;; git-timemachine
   `(git-timemachine-minibuffer-detail-face ((,class (:foreground ,rainbow-8 :inherit bold :background ,bg4))))

;;;;; highlight-symbol
   `(highlight-symbol-face ((,class (:background ,bg2))))

;;;;; hydra
   `(hydra-face-blue ((,class (:foreground ,rainbow-8))))
   `(hydra-face-red ((,class (:foreground ,rainbow-9))))

;;;;; info
   `(info-header-xref ((,class (:foreground ,func :underline t))))
   `(info-menu ((,class (:foreground ,rainbow-6))))
   `(info-node ((,class (:foreground ,func :inherit bold))))
   `(info-quoted-name ((,class (:foreground ,keyword))))
   `(info-reference-item ((,class (:background nil :underline t :inherit bold))))
   `(info-string ((,class (:foreground ,str))))
   `(info-title-1 ((,class (:height 1.4 :inherit bold))))
   `(info-title-2 ((,class (:height 1.3 :inherit bold))))
   `(info-title-3 ((,class (:height 1.3))))
   `(info-title-4 ((,class (:height 1.2))))

;;;;; latex
   `(font-latex-bold-face ((,class (:foreground ,type))))
   `(font-latex-italic-face ((,class (:foreground ,keyword :italic t))))
   `(font-latex-match-reference-keywords ((,class (:foreground ,const))))
   `(font-latex-match-variable-keywords ((,class (:foreground ,var))))
   `(font-latex-sectioning-0-face ((,class (:inherit bold :foreground ,head3 :height 1.3))))
   `(font-latex-sectioning-1-face ((,class (:inherit bold :foreground ,head4 :height 1.3))))
   `(font-latex-sectioning-2-face ((,class (:inherit bold :foreground ,head1 :height 1.3 ))))
   `(font-latex-sectioning-3-face ((,class (:inherit bold :foreground ,head2 :height 1.2))))
   `(font-latex-sectioning-4-face ((,class (:bold nil :foreground ,head3 :height 1.1))))
   `(font-latex-sectioning-5-face ((,class (:bold nil :foreground ,head4))))
   `(font-latex-string-face ((,class (:foreground ,str))))
   `(font-latex-warning-face ((,class (:foreground ,warning))))

;;;;; linum-mode
   `(linum ((,class (:foreground ,fg1 :background ,bg2 :inherit default))))

;;;;; line-numbers
   `(line-number ((,class (:foreground ,fg1 :background ,bg2 :inherit default))))
   `(line-number-current-line ((,class (:foreground ,builtin :background ,bg2 :inherit line-number))))

;;;;; linum-relative
   `(linum-relative-current-face ((,class (:foreground ,type))))

;;;;; man
   `(Man-overstrike ((,class (:foreground ,head1 :inherit bold))))
   `(Man-reverse ((,class (:foreground ,bg3))))
   `(Man-underline ((,class (:foreground ,type :underline t))))

;;;;; perspective
   `(persp-selected-face ((,class (:inherit bold :foreground ,func))))

;;;;; spaceline
   `(spaceline-highlight-face ((,class (:background ,bg1 :foreground ,fg1))))
   `(spaceline-flycheck-error  ((,class (:foreground ,err))))
   `(spaceline-flycheck-info   ((,class (:foreground ,keyword))))
   `(spaceline-flycheck-warning((,class (:foreground ,warning))))
   `(spaceline-python-venv ((,class (:foreground ,type))))

;;;;; powerline
   `(powerline-active1 ((,class (:background ,bg2 :foreground ,fg2))))
   `(powerline-active2 ((,class (:background ,bg1 :foreground ,fg1))))
   `(powerline-inactive1 ((,class (:background ,bg2 :foreground ,comment))))
   `(powerline-inactive2 ((,class (:background ,bg1 :foreground ,comment))))

;;;;; spacemacs-specific
   `(spacemacs-transient-state-title-face ((,class (:background nil :foreground ,type :box nil :inherit bold))))

;;;;; ivy
   ;; `(ivy-current-match ((,class (:background ,var :foreground ,bg1))))
   ;; `(ivy-minibuffer-match-face-1 ((,class (:inherit bold))))
   ;; `(ivy-minibuffer-match-face-2 ((,class (:foreground ,head1 :underline t))))
   ;; `(ivy-minibuffer-match-face-3 ((,class (:foreground ,head4 :underline t))))
   ;; `(ivy-minibuffer-match-face-4 ((,class (:foreground ,head3 :underline t))))
   ;; `(ivy-remote ((,class (:foreground ,rainbow-2))))

;;;;; swiper
   ;; `(swiper-line-face ((,class (:background ,var :foreground ,bg1))))
   ;; `(swiper-match-face-1 ((,class (:inherit bold))))
   ;; `(swiper-match-face-2 ((,class (:foreground ,markup :underline t))))
   ;; `(swiper-match-face-3 ((,class (:foreground ,head4 :underline t))))
   ;; `(swiper-match-face-4 ((,class (:foreground ,head3 :underline t))))

;;;;; which-key
   `(which-key-command-description-face ((,class (:foreground ,builtin))))
   `(which-key-group-description-face ((,class (:foreground ,keyword))))
   `(which-key-key-face ((,class (:foreground ,func :inherit bold))))
   `(which-key-separator-face ((,class (:background nil :foreground ,str))))
   `(which-key-special-key-face ((,class (:background ,func :foreground ,bg1))))

;;;;; whitespace-mode
   `(whitespace-empty ((,class (:background nil :foreground ,rainbow-7))))
   `(whitespace-indentation ((,class (:background nil :foreground ,warning))))
   `(whitespace-line ((,class (:background nil :foreground ,type))))
   `(whitespace-newline ((,class (:background nil :foreground ,type))))
   `(whitespace-space ((,class (:background nil :foreground ,builtin))))
   `(whitespace-space-after-tab ((,class (:background nil :foreground ,rainbow-7))))
   `(whitespace-space-before-tab ((,class (:background nil :foreground ,rainbow-7))))
   `(whitespace-tab ((,class (:background nil :foreground ,builtin))))
   `(whitespace-trailing ((,class (:background ,err :foreground ,fg2))))
;;;;; snails

   `(snails-header-line-face ((,class (:inherit font-lock-function-name-face :underline t :height 1.2))))
   `(snails-header-index-face ((,class (:inherit font-lock-function-name-face :underline t))))
   `(snails-input-buffer-face ((,class (:height 300))))

;;;;; other, need more work
   `(undo-tree-visualizer-current-face ((,class :foreground ,keyword)))
   `(undo-tree-visualizer-default-face ((,class :foreground ,builtin)))
   `(undo-tree-visualizer-register-face ((,class :foreground ,type)))
   `(undo-tree-visualizer-unmodified-face ((,class :foreground ,var)))
   ))


;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'purifier)

;; Local Variables:
;; no-byte-compile: t
;; End:

;;; purifier-theme.el ends here
