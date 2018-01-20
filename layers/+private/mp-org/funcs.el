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
  (run-with-idle-timer 300 t 'mp-org/auto-org-agenda-task)

  ;; Switch auto org agenda task
  (spacemacs/set-leader-keys "oa" 'mp-org/switch-auto-org-agenda-task)

  ;; Count Words
  (spacemacs/set-leader-keys "xC" 'advance-words-count)

  ;; Org Agenda Reload
  (spacemacs/set-leader-keys "or" 'mp-org/org-agenda-reload-files)

  ;; Org new file in Dropbox
  (spacemacs/set-leader-keys "on" 'mp-org/new-org-buffer-in-dropdire)

  ;; Uploat img link file
  (spacemacs/set-leader-keys "om" 'mp-org/mequ-upload-img-link-file)

  ;; Wraper
  (spacemacs/declare-prefix "ow" "wrapper")
  (spacemacs/set-leader-keys "wi" 'mp-org/wrap-math-inline-formula)
  (spacemacs/set-leader-keys "wb" 'mp-org/wrap-math-block-formula)

  ;; (spacemacs/set-leader-keys "xf" 'fill-region)
  ;; This function is the same as 'gq' in evil(vim)
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

;;;; Mequ And Custom Link

(defun mp-org/get-filepath-at-org-img-link ()
  "Return the absolute path of img link in org buffer."
  (let (p1 p2)
    (skip-chars-backward "^[")
    (setq p1 (point))
    (skip-chars-forward "^]")
    (setq p2 (point))
    (setq imglink (string-trim (buffer-substring-no-properties p1 p2)))
    (when (string-prefix-p "img:" imglink)
      (expand-file-name (substring imglink 4))
      ))
  )

(defun mp-org/mequ-upload-file (filepath)
  (let ((command-str (format "mequ -c %s -f \"%s\"" mequ-conf-file filepath)))
    (shell-command-to-string command-str)))

(defun mp-org/mequ-upload-img-link-file ()
  "From img link it get file path, then upload to qiniu."
  (interactive)
  (message (mp-org/mequ-upload-file (mp-org/get-filepath-at-org-img-link))))


(defun mp-org/custom-link-img-follow (path)
  "Click event of custom link img."
  (org-open-file-with-emacs path)
   )

(defun mp-org/custom-link-img-export (path desc format)
  "export event of custom link img."
  ;; concat custom img host and final two section of the path
  (let (uri-slices slices-len url)
    (setq uri-slices (split-string path "/"))
    (setq slices-len (length uri-slices))
    (setq url (if (< slices-len 2)
                  (concat custom-link-img-export-host "/" (car uri-slices))
                (concat custom-link-img-export-host "/"
                        (nth (- slices-len 2) uri-slices) "/"
                        (nth (- slices-len 1) uri-slices))))
    (message (url-encode-url url))
    (cond
     ((eq format 'html)
      (format "<img src=\"%s\" alt=\"%s\"/>" (url-encode-url url) desc))
     ((eq format 'md)
      (format "![%s](%s)" desc (url-encode-url url)))
     )
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

;;;; Toggle Inline Image

;; Codes is modified from 'org.el'
;; display-inline-images will display images whoes line type is 'file' or 'img'

(defun mp-org/toggle-inline-images (&optional include-linked)
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `mp-org/display-inline-images'."
  (interactive "P")
  (if org-inline-image-overlays
      (progn
        (org-remove-inline-images)
        (when (called-interactively-p 'interactive)
          (message "Inline image display turned off")))
    (mp-org/display-inline-images include-linked)
    (when (called-interactively-p 'interactive)
      (message (if org-inline-image-overlays
                   (format "%d images displayed inline"
                           (length org-inline-image-overlays))
                 "No images to display inline")))))

(defun mp-org/display-inline-images (&optional include-linked refresh beg end)
  "Display inline images.
TODO: write a patch or request a issue for multi link types

Same as 'org-display-inline-images', except img link type."
  (interactive "P")
  (when (display-graphic-p)
    (unless refresh
      (org-remove-inline-images)
      (when (fboundp 'clear-image-cache) (clear-image-cache)))
    (org-with-wide-buffer
     (goto-char (or beg (point-min)))
     (let* ((case-fold-search t)
	    (file-extension-re (image-file-name-regexp))
	    (link-abbrevs (mapcar #'car
				  (append org-link-abbrev-alist-local
					  org-link-abbrev-alist)))
	    ;; Check absolute, relative file names and explicit
	    ;; "img:" or “file:" links.  Also check link abbreviations since
	    ;; some might expand to "file" links.
	    (file-types-re (format "[][]\\[\\(?:img\\|file\\|[./~]%s\\)"
				   (if (not link-abbrevs) ""
				     (format "\\|\\(?:%s:\\)"
					     (regexp-opt link-abbrevs))))))
       (while (re-search-forward file-types-re end t)
	 (let ((link (save-match-data (org-element-context))))
	   ;; Check if we're at an inline image, i.e., an image file
	   ;; link without a description (unless INCLUDE-LINKED is
	   ;; non-nil).
	   (when (and (or (equal "img" (org-element-property :type link))
                    (equal "file" (org-element-property :type link)))
		      (or include-linked
			  (null (org-element-contents link)))
		      (string-match-p file-extension-re
				      (org-element-property :path link)))
	     (let ((file (expand-file-name
			  (org-link-unescape
			   (org-element-property :path link)))))
	       (when (file-exists-p file)
		 (let ((width
			(cond
			 ((not (image-type-available-p 'imagemagick)) nil)
			 ((eq org-image-actual-width t) nil)
			 ((listp org-image-actual-width)
			  (or
			   (let ((paragraph
				  (let ((e link))
				    (while (and (setq e (org-element-property
							 :parent e))
						(not (eq (org-element-type e)
							 'paragraph))))
				    e)))
			     (when paragraph
			       (save-excursion
				 (goto-char (org-element-property :begin paragraph))
				 (when
				     (re-search-forward
				      "^[ \t]*#\\+attr_.*?: +.*?:width +\\(\\S-+\\)"
				      (org-element-property
				       :post-affiliated paragraph)
				      t)
				   (string-to-number (match-string 1))))))
			   (car org-image-actual-width)))
			 ((numberp org-image-actual-width)
			  org-image-actual-width)))
		       (old (get-char-property-and-overlay
			     (org-element-property :begin link)
			     'org-image-overlay)))
		   (if (and (car-safe old) refresh)
		       (image-refresh (overlay-get (cdr old) 'display))
		     (let ((image (create-image file
						(and width 'imagemagick)
						nil
						:width width)))
		       (when image
			 (let ((ov (make-overlay
				    (org-element-property :begin link)
				    (progn
				      (goto-char
				       (org-element-property :end link))
				      (skip-chars-backward " \t")
				      (point)))))
			   (overlay-put ov 'display image)
			   (overlay-put ov 'face 'default)
			   (overlay-put ov 'org-image-overlay t)
			   (overlay-put
			    ov 'modification-hooks
			    (list 'org-display-inline-remove-overlay))
			   (push ov org-inline-image-overlays)))))))))))))))

;;; mp-org/funcs.el ends here
