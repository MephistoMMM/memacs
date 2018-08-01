;;; mp-org/config.el --- provide configs for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keywords

;;; Commentary:

;;

;;; Code:

;; mrg path
(defvar notes-org-directory-path "~/.notes_org"
  "Directroy for store notes.")

(setq close-auto-org-agenda-task t)

;; bibtex
(setq org-ref-default-bibliography '("~/Dropbox/Papers/references.bib")
      org-ref-pdf-directory "~/Dropbox/Papers/"
      org-ref-bibliography-notes "~/Dropbox/Papers/notes.org")

;;; mp-org/config.el ends here
