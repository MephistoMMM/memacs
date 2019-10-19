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

(defun memacs//mission-start-find-file-name (nodirectory)
  (let ((full-file-name
         (ivy-read "Find file: " #'read-file-name-internal
                   :matcher #'counsel--find-file-matcher
                   :action (lambda (x) (kill-new
                                        (expand-file-name
                                         (if (stringp x) x (car x))
                                         ivy--directory)))
                   :preselect (counsel--preselect-file)
                   :require-match 'confirm-after-completion
                   :keymap counsel-find-file-map
                   :caller 'counsel-find-file)))
    (if nodirectory
        (file-name-nondirectory full-file-name)
      full-file-name
      ))
  )

(defun memacs//mission-start-candidates-function (str pred _)
  (mapcar (lambda (mission)
            (propertize (car mission) 'property (cdr mission)))
            memacs-mission-starter-mission-list))

(defun memacs/mission-starter-start(mission)
  "Select a mission to start from memacs-mission-start-mission-list."
  (interactive
   (list (ivy-completing-read "MISSIONS:"
    #'memacs//mission-start-candidates-function nil t)))
  (let ((mode (nth 0 (get-text-property 0 'property mission)))
        (path (nth 1 (get-text-property 0 'property mission)))
        (file (nth 2 (get-text-property 0 'property mission))))
    (switch-to-buffer (generate-new-buffer mission))
    (call-interactively mode)
    (setq default-directory (if (stringp path) path (eval path)))
    (when (or (stringp file) (listp file))
      (let ((visited-file-name (if (stringp file)
                                   (concat default-directory "/" file)
                                 (eval file))))
        (set-visited-file-name visited-file-name)
        (if (and (file-name-directory visited-file-name)
                 (not (string= (file-name-directory visited-file-name)
                               default-directory)))
            (setq default-directory (file-name-directory visited-file-name)))
        ))
    )
    ;; TODO create a mission-start-buffer-init-hook
    (auto-insert)
  )

(defun memacs//mission-help-candidates-function (str pred _)
  (mapcar (lambda (help)
            (propertize (car help) 'subhelp (cdr help)))
          memacs-mission-helper-help-list))

(defun memacs/mission-helper-help(help)
  "Select a mission to start from memacs-mission-start-mission-list."
  (interactive
   (list (let ((subhelp
                (get-text-property
                 0 'subhelp
                 (ivy-completing-read "HELP LIST:"
                                      #'memacs//mission-help-candidates-function
                                      nil t))))
           (ivy-completing-read "SUBHELP LIST:"
                                (lambda (str pred _)
                                  (mapcar (lambda (shp)
                                            (propertize
                                             (car shp)
                                             'url (cdr shp)))
                                          subhelp))
                                nil t)
           )))
  (let ((url (nth 0 (get-text-property 0 'url help))))
    (if url
        (browse-url url browse-url-new-window-flag)
      (error "No URL found"))
    )
  )


;;;; Source Code

(defconst mp-org/src-code-types
  '("lua" "emacs-lisp" "python" "c" "shell" "java" "js" "clojure" "c++" "css" "go" "rust" "sh" "sass" "sql" "awk" "haskell" "latex" "lisp" "matlab" "org" "perl" "ruby" "scheme" "sqlite" "yaml"))
(defvar mp-org--org-insert-src-code-block-history nil)

(defun mp-org/org-insert-src-code-block (src-code-type)
  "Insert a `SRC-CODE-TYPE' type source code block in org-mode.
Go files should disable fly-check."
  (interactive (list
                (ivy-completing-read
                 "Source code type: "
                 mp-org/src-code-types nil nil
                 (car mp-org--org-insert-src-code-block-history)
                 'mp-org--org-insert-src-code-block-history
                 )))
  (progn
    (newline-and-indent)
    (insert (format "#+BEGIN_SRC %s\n" src-code-type))
    (newline-and-indent)
    (insert "#+END_SRC\n")
    (previous-line 2)
    (org-edit-src-code)))

(defvar mp-org--wrap-previous-function nil
  "Store previous wrap function.")
(defvar mp-org--wrap-call-from-resume nil
  "Record current function is whether calling from `mp-org/wrap-resume'.")
(defun mp-org/wrap-resume (start end)
  "Resume previous function from `mp-org--wrap-previous-function', while
set `mp-org--wrap-call-from-resume' to t."
  (interactive "r")
  ;; do nothing if `mp-org--wrap-previous-function' is nil
  (unless (not mp-org--wrap-previous-function)
    (setq mp-org--wrap-call-from-resume t)
    (funcall mp-org--wrap-previous-function start end)
    (setq mp-org--wrap-call-from-resume nil))
  )
(defun mp-org//wrap-save-previous-fuction (orig-fun &rest args)
  "Save orig-fun to `mp-org--wrap-previous-function'. This is a advice function,
please use it like `(advice-add 'fun
                        :around #'mp-org//wrap-save-previous-fuction)'"
  (call-interactively orig-fun)
  (setq mp-org--wrap-previous-function orig-fun)
  )

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
(advice-add 'mp-org/wrap-math-block-formula :around #'mp-org//wrap-save-previous-fuction)

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
(advice-add 'mp-org/wrap-math-inline-formula :around #'mp-org//wrap-save-previous-fuction)

(defun mp-org/wrap-source-code (start end)
  "Insert '#+BEGIN_SRC lang' and '#+END_SRC' to the begin and end of code"
  (interactive "r")
  (let ((lang (if mp-org--wrap-call-from-resume
                  (car mp-org--org-insert-src-code-block-history)
                (ivy-completing-read
                 "Source code type: "
                 mp-org/src-code-types nil nil
                 (car mp-org--org-insert-src-code-block-history)
                 'mp-org--org-insert-src-code-block-history
                 )
                )))
    (save-excursion
      (narrow-to-region start end)
      (goto-char (point-min))
      (insert (format "#+BEGIN_SRC %s\n" lang))
      (goto-char (point-max))
      (insert "#+END_SRC\n")
      (widen))
    )
  )
(advice-add 'mp-org/wrap-source-code :around #'mp-org//wrap-save-previous-fuction)

(defun mp-org/wrap-unordered-list (start end)
  "Insert '- ' to the begin of each line."
  (interactive "r")
  (save-excursion
    (narrow-to-region start end)
    (goto-char (point-max))
    (while (> (forward-line -1) -1) (insert "- "))
    (widen))
  )
(advice-add 'mp-org/wrap-unordered-list :around #'mp-org//wrap-save-previous-fuction)

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
(advice-add 'mp-org/wrap-ordered-list :around #'mp-org//wrap-save-previous-fuction)

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
(advice-add 'mp-org/wrap-quote :around #'mp-org//wrap-save-previous-fuction)

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
(advice-add 'mp-org/wrap-link :around #'mp-org//wrap-save-previous-fuction)

(defun mp-org//linkp (linkstr)
  "Test for link line."
  (string-match "^\\(http\\|file\\|https\\|img\\|ftp\\|ftps\\):" (substring-no-properties linkstr)))

(defun mp-org//link-switch ()
  "Filter link lines in counsel kills, if used ivy layer,
otherwise in kill-rang."
  (if (configuration-layer/layer-usedp 'ivy)
      (seq-filter 'mp-org//linkp (counsel--yank-pop-kills))
      (seq-filter 'mp-org//linkp kill-ring))
  )


;;;; Search Notes
(defun mp-org/color-rg-search-org-notes ()
  "Search all notes in ord directory by color-rg"
  (interactive)
  (color-rg/slim-color-rg-transient-state/body
   (color-rg-read-input) org-directory)
  )

;;; mp-org/funcs.el ends here
