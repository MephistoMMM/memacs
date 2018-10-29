;;; mp-org/config.el --- provide configs for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keywords

;;; Commentary:

;;

;;; Code:

(defvar memacs-mission-starter-mission-list
      '(
        ;; config new org file in dropbox
        ("Dropbox Note Buffer" org-mode (concat org-directory "/notes") nil)
        )
      "Mission list. Format (buffer-name mode path file-name). if file-name is non-nil, it will set vistied file name.")

(defvar memacs-mission-helper-help-list
  '(
    ("Projectile" ("ignoring files" "https://projectile.readthedocs.io/en/latest/usage/#ignoring-files"))
    )
  "Help list. Format (help-item-name ((help-subitem-name url))).")

;; bibtex
(setq org-ref-default-bibliography '("~/Dropbox/Papers/references.bib")
      org-ref-pdf-directory "~/Dropbox/Papers/"
      org-ref-bibliography-notes "~/Dropbox/Papers/notes.org")

;; mark org-local-directory and org-capture safe
(put 'eval 'safe-local-variable
      (lambda (x)
        (if (listp x) (eq (car x) 'setq-local) nil)
        ))
(put 'org-local-directory 'safe-local-variable #'stringp)
(put 'org-capture-templates 'safe-local-variable #'listp)

;;; mp-org/config.el ends here
