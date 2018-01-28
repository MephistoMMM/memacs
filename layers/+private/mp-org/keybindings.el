;;; mp-org/keybindings.el --- provide keybindings for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keybindings mp-org

;;; Code:

(spacemacs/set-leader-keys

  ;; Switch auto org agenda task
  "oa" 'mp-org/switch-auto-org-agenda-task

  ;; Count Words
  "xC" 'advance-words-count

  ;; Count page lines
  "xll" 'count-lines-page

  ;; Org Agenda Reload
  "or" 'mp-org/org-agenda-reload-files

  ;; Org new file in Dropbox
  "on" 'mp-org/new-org-buffer-in-dropdire

  ;; Uploat img link file
  "om" 'mp-org/mequ-upload-img-link-file)

;; Wraper
(spacemacs/declare-prefix "iw" "wrapper")
(spacemacs/set-leader-keys
  "iww" 'mp-org/wrap-math-inline-formula
  "iwb" 'mp-org/wrap-math-block-formula)

;; (spacemacs/set-leader-keys "xf" 'fill-region)
;; This function is the same as 'gq' in evil(vim)

;;; mp-org/keybindings.el ends here
