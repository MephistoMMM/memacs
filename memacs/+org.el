;;; memacs/+org.el -*- lexical-binding: t; -*-


(setq memacs-org-agenda-todo-view
      `(" " "test Agenda"
        ((agenda ""
                 ((org-agenda-span 'day)
                  (org-deadline-warning-days 365)))
         (tags-todo "+Inbox"
               ((org-agenda-overriding-header "To Refile")
                (org-agenda-files '(,(concat org-directory "/" +org-capture-todo-file)))))
         (todo "TODO"
               ((org-agenda-overriding-header "Emails")
                (org-agenda-files '(,(concat org-directory "/" +org-capture-todo-file)))))
         (todo "NEXT"
               ((org-agenda-overriding-header "In Progress")
                (org-agenda-files '(,(concat org-directory "/" +org-capture-todo-file)))
                ))
         (tags-todo "+Projects"
               ((org-agenda-overriding-header "Projects")
                (org-agenda-files '(,(concat org-directory "/" +org-capture-todo-file)))
                ))
         (todo "TODO"
               ((org-agenda-overriding-header "One-off Tasks")
                (org-agenda-files '(,(concat org-directory "/" +org-capture-todo-file)))
                (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))
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
