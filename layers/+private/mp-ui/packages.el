;;; mp-ui/packages.el --- defines packages for ui

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: ui packages spacemacs elisp

;;; Commentary:

;; description

;;; Code:

(defconst mp-ui-packages
  '(
    tern
    hindent
    ranger)
 )

(defun mp-ui/post-init-tern ()
  "Change the view of Tern in minor-mode-list"
  (spacemacs|diminish tern-mode " \u24e3" " (js_t) ")
  )

(defun mp-ui/post-init-hindent ()
  "Change the view of Hindent in minor-mode-list"
  (spacemacs|diminish hindent-mode " \u24bd" " (h) ")
  )

;; TODO: it should change golden-ratio-previous-enable to nil while toggle the golden-ratio
(defun mp-ui/post-init-ranger ()
  "Configuations for ranger!"
  (setq ranger-ignored-extensions '("mkv" "iso" "mp4"))
  (setq ranger-max-preview-size 2)
  )

;;; mp-ui/packages.el ends here
