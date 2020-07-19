;;; memacs/+org.el -*- lexical-binding: t; -*-

(defvar memacs-org-export-setup-file "~/Dropbox/dotconf/export_setup.org"
  "Setup file path for org to export as more beautiful result.
Used in org file template")

;; Base
;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/Dropbox/org"
      +org-capture-work-directory "~/Documents/works"
      org-attach-id-dir (expand-file-name "~/Dropbox/org/statics/")
      org-download-image-dir (expand-file-name "~/Dropbox/org/statics/")
      org-roam-db-location (expand-file-name "~/.local/roam/org-roam.db"))

;; Agenda View
(setq memacs-org-agenda-todo-view
      `(" " "test Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-agenda-start-day "+0d")
                  (org-deadline-warning-days 365)))
         (todo "TODO"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '(,(+org-capture-work-todo-file)))))
         ;; (todo "TODO"
         ;;       ((org-agenda-overriding-header "Emails")
         ;;        (org-agenda-files '(,(concat org-directory "/" +org-capture-todo-file)))))
         (alltodo ""
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '(,(expand-file-name "next.org" +org-capture-work-directory)))
                ))
         (todo "PROJ"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(expand-file-name "proj.org" +org-capture-work-directory)))
                ))
         ;; (todo "TODO"
         ;;       ((org-agenda-overriding-header "One-off Tasks")
         ;;        (org-agenda-files '(,(concat org-directory "/" +org-capture-todo-file)))
         ;;        (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
         nil)))

(setq org-agenda-custom-commands nil)
(add-to-list 'org-agenda-custom-commands `,memacs-org-agenda-todo-view)

;; Roam
(defmacro memacs-org-roam-common-head ()
  "Common head for org-roam capture."
  (concat
   "#+SETUPFILE: " memacs-org-export-setup-file
   "\n#+TITLE: ${title}\n#+DATE: %<%Y-%m-%d>\n"))

(setq org-roam-capture-templates
  `(
    ("d" "default" plain #'org-roam-capture--get-point
      "%?" :file-name "%<%Y%m%d%H%M%S>-${slug}" :head ,(memacs-org-roam-common-head) :unnarrowed t)
    ("p" "Programming" plain #'org-roam-capture--get-point
      "%?"
      :file-name "prog/%(memacs-org-roam-complete-program-languages)/${slug}"
      :head ,(memacs-org-roam-common-head) :unnarrowed t)
    ("a" "Algorithm" plain #'org-roam-capture--get-point
      "%?"
      :file-name "algorithm/${slug}"
      :head ,(memacs-org-roam-common-head) :unnarrowed t)
    ("s" "Software" plain #'org-roam-capture--get-point
      "%?"
      :file-name "software/%(memacs-org-roam-complete-software-directories)/${slug}"
      :head ,(memacs-org-roam-common-head) :unnarrowed t)
    ("w" "Wiki" plain #'org-roam-capture--get-point
      "%?"
      :file-name "wiki/${slug}"
      :head ,(memacs-org-roam-common-head) :unnarrowed t)
    ))

;; Export
(defadvice! +org-export-output-file-name-a (orig-fn extension &optional subtreep pub-dir)
  "Modifies org-export to place exported files in a different directory"
  :around #'org-export-output-file-name
  (unless (null memacs--org-export-directory)
      (setq pub-dir memacs--org-export-directory)
      (when (not (file-directory-p pub-dir))
        (make-directory pub-dir)))
  (let ((output (funcall orig-fn extension subtreep pub-dir)))
    (setq memacs--org-export-directory nil)
    output))

;; Helps
(setq memacs-mission-helper-help-list
      '(
        ("Projectile"
          ("ignoring files" "https://projectile.readthedocs.io/en/latest/projects/#ignoring-files"))
        ("Dockerfile"
          ("docker reference" "https://docs.docker.com/engine/reference/builder/#usage")
          ("compose reference" "https://docs.docker.com/compose/compose-file/"))
        ("Golang"
          ("debugger: Delve" "https://github.com/derekparker/delve/blob/master/Documentation/cli/README.md"))
        ("Tools"
          ("RestClient" "https://github.com/pashky/restclient.el"))
        ("Applescript" "http://downloads.techbarrack.com/books/programming/AppleScript/website/index.html")
        )
      )

(defconst memacs-org-meeting-types
  '("组会" "晨会" "周会"))

;; Missions
(setq memacs-mission-starter-mission-list
      '(
        ;; config new org file in dropbox
        ("Dropbox Note Buffer" org-mode
         (concat org-directory "/notes/") (memacs//mission-start-find-file-name nil))
        ("Codewar&Golang" go-mode
         (format-time-string "~/Workspace/go/src/codewar_pg/%Y_%m_%d"))
        ("Daily Meeting" org-mode
         "~/Documents/works/roam/"
         (format "%s_%s.org"
                  (ivy-completing-read "Meeting type: " memacs-org-meeting-types)
                  (format-time-string "%Y%m%d")))
        ))


(map! :leader
      ;; HACK override defalut action of "SPC A"
      :desc "Org Capture"   "A"    (λ!! #'org-agenda nil " ")
      (:prefix-map ("m" . "memacs")
       :desc "starter" "s" #'+memacs/mission-starter-start
       :desc "helper"  "h" #'+memacs/mission-helper-help
       :desc "roam capture" "r" #'org-roam-capture
       ))
