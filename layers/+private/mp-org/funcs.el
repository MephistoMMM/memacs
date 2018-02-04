;;; mp-org/funcs.el --- provide functions for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keywords

;;; Commentary:

;; mp-org/create-mrg-buffer provide the fundament for mrg:
;; - create markdown buffer
;; - change current buffer directory

;;; Code:


;;;; Better Default

(defun mp-org/better-default ()
  "Better default for mp-org, something done in
user-config should be defined in this function!"
  ;; (run-with-idle-timer 300 t 'mp-org/auto-org-agenda-task)
  )


(defun mp-org/new-org-buffer-in-dropdire ()
  "Create a new buffer then init by mrg."
  (interactive)
  (let ((ξbuf (generate-new-buffer "Dropbox org buffer")))
    (switch-to-buffer ξbuf))
  (org-mode)
  (setq default-directory notes-org-directory-path)
  )


;;;; Source Code

(setq mp-org/src-code-types
  '("emacs-lisp" "python" "c" "shell" "java" "js2" "clojure" "c++" "css" "go" "rust" "sh" "sass" "sql" "awk" "haskell" "latex" "lisp" "matlab" "org" "perl" "ruby" "scheme" "sqlite"))

(defun mp-org/org-insert-src-code-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode.
Go files should disable fly-check."
  (interactive (list (ivy-completing-read "Source code type: " mp-org/src-code-types)))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defun mp-org/wrap-math-block-formula (start end)
  "Insert '\\[ ... \\]' to the begin and end of formula"
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (insert "\\[ ")
    (goto-char (point-max))
    (insert " \\]")
    (widen))
  )

(defun mp-org/wrap-math-inline-formula (start end)
  "Insert '\\( ... \\)' or '\\[ ... \\]' to the begin and end of formula"
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (insert "\\( ")
    (goto-char (point-max))
    (insert " \\)")
    (widen))
  )

(defun mp-org/wrap-source-code (start end)
  "Insert '#+BEGIN_SRC lang' and '#+END_SRC' to the begin and end of code"
  (interactive "r")
  (let ((lang (ivy-completing-read "Source code type: " mp-org/src-code-types)))
    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert (format "#+BEGIN_SRC %s\n" lang))
      (goto-char (point-max))
      (insert "#+END_SRC\n")
      (widen))
    )
  )



;;;; Auto Org Agenda

(defun mp-org/org-agenda-reload-files ()
  "Reset the default value of org-agenda-reload-files."
  (interactive)
  (setq-default org-agenda-files (find-lisp-find-files org-directory "\.org$"))
  (message "Reload org files success!")
  )

(defun mp-org/auto-org-agenda-task ()
  "If auto org agenda task is not close.
Switch to @org -> reload org agenda file -> show agenda list"
  (unless close-auto-org-agenda-task
    (spacemacs/custom-perspective-@Org)
    (mp-org/org-agenda-reload-files)
    (org-agenda-list))
  )

(defun mp-org/switch-auto-org-agenda-task ()
  (interactive)
  (setq close-auto-org-agenda-task (not close-auto-org-agenda-task))
  (if close-auto-org-agenda-task
      (message "Closed auto org agenda task.")
      (message "Opened auto org agenda task."))
  )


;;; mp-org/funcs.el ends here
