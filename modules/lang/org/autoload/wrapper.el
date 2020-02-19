;;; lang/org/autoload/wrapper.el -*- lexical-binding: t; -*-

(defconst +org-src-code-types
  '("lua" "emacs-lisp" "python" "c" "shell" "java" "js" "javascript" "clojure" "c++" "css" "go" "rust" "sh" "sass" "sql" "awk" "haskell" "latex" "lisp" "matlab" "org" "perl" "ruby" "scheme" "sqlite" "yaml"))
(defvar +org--org-insert-src-code-block-history nil)

(defvar +org--wrap-previous-function nil
  "Store previous wrap function.")
(defvar +org--wrap-call-from-resume nil
  "Record current function is whether calling from `+org-wrap-resume'.")

(defmacro define-org-region-wrapper! (name comment &rest body)
  "Define a wrapper function for org mode.
Wrap function save excursion first, then narrow to region,
execute logic of body. Finally, widen narrow and put the
wrap function to `+org--wrap-previous-function'"
  `(defun ,name (start end)
     ,comment
    (interactive "r")
    (save-excursion
      (narrow-to-region start end)
      ,@body
      (widen))
    (setq +org--wrap-previous-function #',name)))

;;;###autoload
(defun +org/wrap-resume (start end)
  "Resume previous function from `+org--wrap-previous-function', while
set `+org--wrap-call-from-resume' to t."
  (interactive "r")
  ;; do nothing if `+org--wrap-previous-function' is nil
  (unless (not +org--wrap-previous-function)
    (setq +org--wrap-call-from-resume t)
    (funcall +org--wrap-previous-function start end)
    (setq +org--wrap-call-from-resume nil))
  )

;; (defun +org--wrap-save-previous-fuction (orig-fun &rest args)
;;   "Save orig-fun to `+org--wrap-previous-function'. This is a advice function,
;; please use it like `(advice-add 'fun
;;                         :around #'+org--wrap-save-previous-fuction)'"
;;   (call-interactively orig-fun)
;;   (setq +org--wrap-previous-function orig-fun)
;;   )

;;;###autoload (autoload '+org/wrap-math-block-formula "lang/org/autoload/wrapper" nil t)
(define-org-region-wrapper! +org/wrap-math-block-formula
  "Insert '\\[ ... \\]' to the begin and end of formula"
  (goto-char (point-min))
  (insert "\\[ ")
  (goto-char (point-max))
  (insert " \\]"))

;;;###autoload (autoload '+org/wrap-math-inline-formula "lang/org/autoload/wrapper" nil t)
(define-org-region-wrapper! +org/wrap-math-inline-formula
  "Insert '\\( ... \\)' or '\\[ ... \\]' to the begin and end of formula"
  (goto-char (point-min))
  (insert "\\( ")
  (goto-char (point-max))
  (insert " \\)"))

;;;###autoload (autoload '+org/wrap-source-code "lang/org/autoload/wrapper" nil t)
(define-org-region-wrapper! +org/wrap-source-code
  "Insert '#+BEGIN_SRC lang' and '#+END_SRC' to the begin and end of code"
  (let ((lang (if +org--wrap-call-from-resume
                  (car +org--org-insert-src-code-block-history)
                (ivy-completing-read
                 "Source code type: "
                 +org-src-code-types nil nil
                 (car +org--org-insert-src-code-block-history)
                 '+org--org-insert-src-code-block-history
                 )
                )))
      (goto-char (point-min))
      (insert (format "#+BEGIN_SRC %s\n" lang))
      (goto-char (point-max))
      (insert "#+END_SRC\n"))
  )

;;;###autoload (autoload '+org/wrap-unordered-list "lang/org/autoload/wrapper" nil t)
(define-org-region-wrapper! +org/wrap-unordered-list
  "Insert '- ' to the begin of each line."
    (goto-char (point-max))
    (while (> (forward-line -1) -1) (insert "- ")))

;;;###autoload (autoload '+org/wrap-ordered-list "lang/org/autoload/wrapper" nil t)
(define-org-region-wrapper! +org/wrap-ordered-list
  "Insert '%d. ' to the begin of each line."
    (goto-char (point-min))
    (let* ((lineno 1)
          (linecount (count-lines start end)))
      (while (<= lineno linecount)
        (insert (format "%d. " lineno))
        (forward-line 1)
        (setq lineno (1+ lineno)))))

;;;###autoload (autoload '+org/wrap-quote "lang/org/autoload/wrapper" nil t)
(define-org-region-wrapper! +org/wrap-quote
  "Insert '#+BEGIN_QUOTE' and '#+END_QUOTE' to the begin and end of quote region"
    (goto-char (point-min))
    (insert "#+BEGIN_QUOTE\n")
    (goto-char (point-max))
    (insert "#+END_QUOTE\n"))

;;;###autoload (autoload '+org/wrap-link "lang/org/autoload/wrapper" nil t)
(define-org-region-wrapper! +org/wrap-link
  "Insert '[' , ']' and link string to the begin and end of region."
  (let ((link (ivy-completing-read "Value of link: " (+org--link-switch))))
      (goto-char (point-min))
      (insert (format "[[%s][" link))
      (goto-char (point-max))
      (insert "]]")
      ))

(defun +org--linkp (linkstr)
  "Test for link line."
  (string-match "^\\(http\\|file\\|https\\|img\\|ftp\\|ftps\\):" (substring-no-properties linkstr)))

(defun +org--link-switch ()
  "Filter link lines in counsel kills, if used ivy layer,
otherwise in kill-rang."
  (if (featurep! :completion ivy)
      (seq-filter '+org--linkp (counsel--yank-pop-kills))
      (seq-filter '+org--linkp kill-ring))
  )
