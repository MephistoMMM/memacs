;;; mp-org/keybindings.el --- provide keybindings for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keybindings mp-org

;;; Code:

(spacemacs/set-leader-keys

  ;; Switch auto org agenda task
  "aoT" 'mp-org/switch-auto-org-agenda-task

  ;; Count Words
  "xC" 'advance-words-count

  ;; Count page lines
  "xll" 'count-lines-page

  ;; Insert Spaces to Chinese-English hybrid buffer
  "xp" 'pangu-spacing-space-current-buffer

  ;; Org Agenda Reload
  "aor" 'mp-org/org-agenda-reload-files

  ;; Org new file in Dropbox
  "bNo" 'mp-org/new-org-buffer-in-dropdire

  ;; Uploat img link file
  "am" 'mp-org/mequ-upload-img-link-file)

;; Wraper
(which-key-add-key-based-replacements
  "M-i w" '("wrapper" . "Insert wrapper chars"))
(memacs/define-insert-keybinding
  "ww" 'mp-org/wrap-math-inline-formula
  "wb" 'mp-org/wrap-math-block-formula)

;; (spacemacs/set-leader-keys "xf" 'fill-region)
;; This function is the same as 'gq' in evil(vim)


;; Org
(with-eval-after-load 'org
  (spacemacs/set-leader-keys-for-major-mode 'org-mode
    "C-o" 'mp-org/toggle-inline-images
    "it"  'org-insert-todo-heading
    "ic"  'mp-org/org-insert-src-code-block)

  (spacemacs/declare-prefix-for-mode 'org-mode "w" "wrapper")
  (spacemacs/set-leader-keys-for-major-mode 'org-mode "ws" 'mp-org/wrap-source-code))

(with-eval-after-load 'org-agenda
  (define-key org-agenda-mode-map (kbd "P") 'org-pomodoro))


;; Blog
(spacemacs/set-leader-keys "ma" 'blog-admin-start)


;;; mp-org/keybindings.el ends here
