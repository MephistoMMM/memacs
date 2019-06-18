;;; mp-org/config.el --- provide configs for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keywords

;;; Commentary:

;;

;;; Code:

(defvar org-agenda-files
  '()
  "org-agenda-files")

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

(setq-default
 org-modules
 '(org-bbdb org-habit org-info org-irc org-w3m org-mac-link org-protocol)
 org-capture-templates
 '(("w" "Task" entry
    (file+headline (lambda () (concat org-directory "/TODOs.org")) "Fighting")
    "* TODO [#A] %^{Task}\nSCHEDULED: %t\n")

   ("t" "Todo" entry
    (file+headline (lambda () (concat org-directory "/TODOs.org")) "Play Space")
    "* TODO [#%^{level|B|C}] %?\nSCHEDULED: %t\n%i\n"
    :empty-lines 1)

   ("l" "Links" entry
    (file+headline (lambda () (concat org-directory "/TODOs.org")) "Play Space")
    "* TODO [#C] %? link \t%^g\nCaptured On: %U\n"
    :empty-lines 1)

   ("b" "Books" entry
    (file+headline (lambda () (concat org-directory "/TODOs.org")) "Books")
    "* TODO [#B] %?"
    :empty-lines 1)

   ("n" "Temporary Notes" entry
    (file+headline (lambda () (concat org-directory "/Temp.org")) "Temporary Notes")
    "* %?\n  %i%a\n%U"
    :empty-lines 1)
   )
 )

;;; mp-org/config.el ends here
