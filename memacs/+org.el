;;; memacs/+org.el -*- lexical-binding: t; -*-


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


(map! :leader
      ;; HACK override defalut action of "SPC A"
      :desc "Org Capture"   "A"    (λ!! #'org-agenda nil " "))
