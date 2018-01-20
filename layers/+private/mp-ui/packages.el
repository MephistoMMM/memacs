;;; mp-ui/packages.el --- defines packages for ui

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: ui packages spacemacs elisp

;;; Commentary:

;; description

;;; Code:

(defconst mp-ui-packages
  '(
    linum-relative
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

;;line number
(defun mp-ui/post-init-linum-relative ()
  "Change line format if not in gui"
  (with-eval-after-load 'linum-relative
    (unless (display-graphic-p)
      (setq linum-relative-format "%3s "))
    )
  )
;;; mp-ui/packages.el ends here
