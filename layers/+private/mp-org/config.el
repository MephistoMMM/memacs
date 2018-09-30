;;; mp-org/config.el --- provide configs for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keywords

;;; Commentary:

;;

;;; Code:

(defvar memacs-mission-start-mission-list
      '(
        ;; config new org file in dropbox
        ("Dropbox Note Buffer" org-mode (concat org-directory "/notes") nil)
        )
      "Mission list. Format (buffer-name mode path file-name). if file-name is non-nil, it will set vistied file name.")

(setq close-auto-org-agenda-task t)

;; bibtex
(setq org-ref-default-bibliography '("~/Dropbox/Papers/references.bib")
      org-ref-pdf-directory "~/Dropbox/Papers/"
      org-ref-bibliography-notes "~/Dropbox/Papers/notes.org")

;;; mp-org/config.el ends here
