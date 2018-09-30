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

;; TODO support file name property
(defun memacs/mission-start(mission)
  "Select a mission to start from memacs-mission-start-mission-list."
  (interactive
    (let ((name (ivy-completing-read
                "MISSIONS:"
                memacs-mission-start-mission-list
                ;; memacs-mission-start-mission-list
                nil t)))
      (seq-filter (lambda (x) (string= (car x) name)) memacs-mission-start-mission-list)))
  (let ((name (car mission))
        (mode (car (cdr mission)))
        (path (car (last mission))))
    (let ((ξbuf (generate-new-buffer name)))
      (switch-to-buffer ξbuf))
    (call-interactively mode)
    (setq default-directory (if (stringp path) path (eval path)))
    )
  )


;;;; Source Code

(defconst mp-org/src-code-types
  '("emacs-lisp" "python" "c" "shell" "java" "js2" "clojure" "c++" "css" "go" "rust" "sh" "sass" "sql" "awk" "haskell" "latex" "lisp" "matlab" "org" "perl" "ruby" "scheme" "sqlite" "yaml"))

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

(defun mp-org/wrap-unordered-list (start end)
  "Insert '- ' to the begin of each line."
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-max))
    (while (> (forward-line -1) -1) (insert "- "))
    (widen))
  )

(defun mp-org/wrap-ordered-list (start end)
  "Insert '%d. ' to the begin of each line."
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (let* ((lineno 1)
          (linecount (count-lines start end)))
      (while (<= lineno linecount)
        (insert (format "%d. " lineno))
        (forward-line 1)
        (setq lineno (1+ lineno))))
    (widen))
  )

(defun mp-org/wrap-quote (start end)
  "Insert '#+BEGIN_QUOTE' and '#+END_QUOTE' to the begin and end of quote region"
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-min))
    (insert "#+BEGIN_QUOTE\n")
    (goto-char (point-max))
    (insert "#+END_QUOTE\n")
    (widen))
  )

(defun mp-org/wrap-link (start end)
  "Insert '[' , ']' and link string to the begin and end of region."
  (interactive "r")
  (let ((link (ivy-completing-read "Value of link: " (mp-org//link-switch))))
    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert (format "[[%s][" link))
      (goto-char (point-max))
      (insert "]]")
      (widen)))
  )

(defun mp-org//linkp (linkstr)
  "Test for link line."
  (string-match "^\\(http\\|file\\|https\\|img\\):" (substring-no-properties linkstr)))

(defun mp-org//link-switch ()
  "Filter link lines in counsel kills, if used ivy layer,
otherwise in kill-rang."
  (if (configuration-layer/layer-usedp 'ivy)
      (seq-filter 'mp-org//linkp (counsel--yank-pop-kills))
      (seq-filter 'mp-org//linkp kill-ring))
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
