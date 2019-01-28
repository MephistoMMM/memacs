;;; mp-org/keybindings.el --- provide keybindings for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keybindings mp-org

;;; Code:

(spacemacs/set-leader-keys

  ;; Count Words
  "xC" 'advance-words-count

  ;; Count page lines
  "xll" 'count-lines-page

  ;; Insert Spaces to Chinese-English hybrid buffer
  "xp" 'pangu-spacing-space-current-buffer

  ;; Mission Start
  "bno" 'memacs/mission-starter-start

  ;; Mission Help
  "hm" 'memacs/mission-helper-help
  )


;;;; Notes
;; Wraper
(memacs/declare-prefix-for-special-leader-key
  memacs--insert-map-keybinding-prefix "w"
  "wrapper" "Insert wrapper chars")
(memacs/define-insert-keybinding
  "ww" 'mp-org/wrap-math-inline-formula
  "wb" 'mp-org/wrap-math-block-formula)

(when (configuration-layer/package-used-p 'color-rg)
  (which-key-add-key-based-replacements
    (concat dotspacemacs-leader-key " m o")
    '("org notes searcher" . "Org notes searcher"))
  (spacemacs/set-leader-keys
    "mo" 'mp-org/color-rg-search-org-notes))

;; (spacemacs/set-leader-keys "xf" 'fill-region)
;; This function is the same as 'gq' in evil(vim)


;;;; Org
(with-eval-after-load 'org
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "C-o" 'memacs-img-toggle-inline-images
    "it"  'org-insert-todo-heading
    "is"  'mp-org/org-insert-src-code-block

    ;; Uploat img link file
    "C-u" 'memacs-img-mequ-upload-img-link-file)

  (spacemacs/declare-prefix-for-mode 'org-mode "w" "wrapper")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "wr" 'mp-org/wrap-resume
    "wq" 'mp-org/wrap-quote
    "wl" 'mp-org/wrap-link
    "wo" 'mp-org/wrap-ordered-list
    "wu" 'mp-org/wrap-unordered-list
    "ws" 'mp-org/wrap-source-code))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;;;; Blog
(spacemacs/set-leader-keys "ma" 'blog-admin-start)


;;;; Bookmark
(spacemacs/set-leader-keys
  "fBs" 'bookmark-set
  "fBS" 'bookmark-set-no-overwrite
  "fBd" 'bookmark-delete
  "mm"  'bookmark-bmenu-list
  )

;;; mp-org/keybindings.el ends here
