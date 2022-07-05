;;; memacs/+org.el -*- lexical-binding: t; -*-

(defvar memacs-org-export-setup-file "~/Dropbox/dotconf/export_setup.org"
  "Setup file path for org to export as more beautiful result.
Used in org file template")

;; Base
;; If you intend to use org, it is recommended you change this!
(setq org-directory "~/Dropbox/org"
      +org-capture-work-directory "~/Documents/works"
      org-attach-id-dir (expand-file-name "~/Dropbox/org/statics/")
      org-download-image-dir (expand-file-name "~/Dropbox/org/statics/"))

;; Export
(setq org-pandoc-options-for-latex-pdf
      '((pdf-engine . "/Library/TeX/texbin/xelatex")))

;; Agenda View
(setq memacs-org-agenda-todo-view
      `(" " "test Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-agenda-start-day "+0d")
                  (org-deadline-warning-days 365)))
         (todo "PROJ"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(expand-file-name "proj.org" +org-capture-work-directory)))
                ))
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
   "#+TITLE: ${title}
#+TITLE_CN: ${title_cn}
#+DATE: %<%Y-%m-%d>
"))

(setq org-roam-db-node-include-function
      (lambda ()
        (not (member "ATTACH" (org-get-tags)))))

(setq org-roam-buffer-width 0.2
      org-roam-db-location (expand-file-name "~/.local/roam/org-roam.db"))

(setq org-roam-capture-templates
  `(
    ("d" "draft" plain "%?"
     :target (file+head "draft/%<%Y%m%d%H%M%S>-${slug}.org"
                        ,(memacs-org-roam-common-head))
     :unnarrowed t)
    ("p" "Programming" plain "%?"
     :target (file+head "prog/%(memacs-org-roam-complete-program-languages)/${slug}.org"
                        ,(memacs-org-roam-common-head))
     :unnarrowed t)
    ("o" "OS" plain "%?"
     :target (file+head "os/%^{OS|linux|macOS}/${slug}.org"
                        ,(memacs-org-roam-common-head))
     :unnarrowed t)
    ("a" "Algorithm" plain "%?"
     :target (file+head "algorithm/${slug}.org"
                        ,(memacs-org-roam-common-head))
     :unnarrowed t)
    ("s" "Software" plain "%?"
     :target (file+head "software/%(memacs-org-roam-complete-software-directories)/${slug}.org"
                        ,(memacs-org-roam-common-head))
     :unnarrowed t)
    ("w" "Wiki" plain "%?"
     :target (file+head "wiki/${slug}.org"
                        ,(memacs-org-roam-common-head))
     :unnarrowed t)
    ("t" "Thoughts" plain "%?"
     :target (file+head "thought/${slug}.org"
                        ,(memacs-org-roam-common-head))
     :unnarrowed t)
    ))

;; Export
(defun memacs-org-export-add-header (backend)
  "Add header in current buffer."
  (let ((setupfile (concat "export_setup_" (symbol-name backend) ".org")))
    (when (file-exists-p! setupfile "~/Dropbox/dotconf/")
      (save-excursion
        (goto-char (point-min))
        (insert "#+SETUPFILE: " "~/Dropbox/dotconf/" setupfile "\n"))
      ))
  (case backend
    ('pandoc (+org-export-parse-and-replace-tables))
    ('latex (+org-export-parse-and-replace-tables)))
  )

(defadvice! +org-export-output-file-name-a (orig-fn extension &optional subtreep pub-dir)
  "Modifies org-export to place exported files in a different directory"
  :around #'org-export-output-file-name
  (unless (null memacs--org-export-directory)
    (setq pub-dir memacs--org-export-directory)
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir)))
  (funcall orig-fn extension subtreep pub-dir))

(defadvice! +org-export-attach-export-links-a (orig-fn &rest _)
  "Modifies org-attach-expand-links to my own implement"
  :around #'org-attach-expand-links
  (memacs-org-attach-expand-links _))

(add-hook 'org-export-before-parsing-hook 'memacs-org-export-add-header)

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
         (format "~/Documents/works/roam/%s_%s.org"
                  (ivy-completing-read "Meeting type: " memacs-org-meeting-types)
                  (format-time-string "%Y%m%d")))
        ))

(setq +org-capture-work-project-todo-template
      (concat "* PROJ %^{Task}\n"
              "** 启动\n"
                "*** 目标\n"
                "- 时间: \n"
                "- 成本: \n"
                "- 质量: \n"
                "*** 范围\n"
                "- 产品范围: \n"
                "- 项目范围: \n"
                "*** 可交付成果\n"
                "*** 关键里程碑\n"
                "*** 风险\n"
                "*** 团队分工\n"
              "** 计划\n"
              "** 风险控制\n"
              "** 收尾\n"))



(map! :leader
      ;; HACK override defalut action of "SPC A"
      :desc "Org Agenda"   "A"    (λ! (org-agenda nil " "))
      (:prefix-map ("m" . "memacs")
       :desc "starter" "s" #'+memacs/mission-starter-start
       :desc "helper"  "h" #'+memacs/mission-helper-help
       :desc "roam capture" "r" #'org-roam-capture
       ))
