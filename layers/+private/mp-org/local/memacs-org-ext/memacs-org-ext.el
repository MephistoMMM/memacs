;;; memacs-org-ext.el --- package provides methods for define img custom link

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: org img customlink

;;; License:

;; GPLv3

;;; Code:

(require 'org)
(require 'ox)


;;;; Mequ

(defvar mequ-conf-file "~/Dropbox/dotconf/mequ.conf"
  "Configuration file about mequ.")

(defvar custom-link-img-export-host "http://qiniu.com"
  "Host of exported img link.")

;;;###autoload
(defun memacs-img-mequ-upload-img-link-file ()
  "From img link it get file path, then upload to qiniu."
  (interactive)
  (message (memacs-img--mequ-upload-file (memacs-img--get-filepath-at-org-img-link))))

(defun memacs-img--get-filepath-at-org-img-link ()
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

(defun memacs-img--mequ-upload-file (filepath)
  (let ((command-str (format "mequ -c %s -f \"%s\"" mequ-conf-file filepath)))
    (shell-command-to-string command-str)))

(defun memacs-img--custom-link-img-follow (path)
  "Click event of custom link img."
  (org-open-file-with-emacs path))


;; org export
(defvar memacs--org-export-directory (expand-file-name "~/Desktop"))
(defun memacs/org-export-dispatch (&optional arg)
  "Change exported destination to the special path."
  (interactive "P")
  (let ((dest (read-directory-name "Export to Directory: "
                                   nil default-directory nil)))
    (setq memacs--org-export-directory (if (string-suffix-p "/" dest)
                                           dest (concat dest "/"))
          memacs-img/before-export-aspect
          #'memacs-img//copy-to-destination-statics-dir)
    (org-export-dispatch))
  )

(defadvice org-export-output-file-name (before org-add-export-dir activate)
  "Modifies org-export to place exported files in a different directory"
  (when (not pub-dir)
    (setq pub-dir memacs--org-export-directory)
    (when (not (file-directory-p pub-dir))
      (make-directory pub-dir))))

;; export files in dired or directory
(defmacro memacs||export-org-in-dired (type org-export-func)
  "Macro to export org files in a dired or directory."
  `(defun ,(intern (format "memacs/export-org-in-dired-to-%s" type)) ()
     ,(format "Export org files in a dired or directory to %s." type)
     (interactive)
     (let ((files
            (if (eq major-mode 'dired-mode)
                (dired-get-marked-files)
              (let ((default-directory (read-directory-name "Source Directory: ")))
                (mapcar #'expand-file-name
                        (file-expand-wildcards "*.org")))))
           (dest (read-directory-name "Export to Directory: "
                                      nil default-directory nil))
           )
       (setq memacs--org-export-directory dest
             memacs-img/before-export-aspect
             #'memacs-img//copy-to-destination-statics-dir)
       (mapc
        (lambda (f)
          (with-current-buffer
              (find-file-noselect f)
            (,org-export-func)))
        files))
     )
  )

;; html
(memacs||export-org-in-dired "html" org-html-export-to-html)


;;;; Custom Link

(defun memacs-img//copy-to-destination-statics-dir (path desc format)
  "A aspect for copy src file to statics dir in destination."
  (let ((uri-slices (split-string path "/"))
        (abs-path (expand-file-name path))
        (statics-dir (concat memacs--org-export-directory "statics/")))
    (if (not (file-exists-p abs-path))
        ;; return nil if image does not exist
        nil
      ;; modify url and copy image file if image exists
      (unless (file-exists-p statics-dir)
        (make-directory statics-dir))
      (let ((dest (concat statics-dir (nth (- (length uri-slices) 1) uri-slices)))
            (relative-path (concat "./statics/" (nth (- (length uri-slices) 1) uri-slices))))
        (unless (file-exists-p dest)
          (copy-file abs-path dest))
        relative-path)))
  )

(defun memacs-img//export-to-Internet (path desc format)
  "A aspect for change path to url in the internet."
  (let ((index (string-match "statics/" path)))
    (if (not index)
        ;; index is nil
        (concat custom-link-img-export-host "/statics/not_found.png")
      ;; index is the offset of "statics/"
      (concat custom-link-img-export-host "/" (substring path index))
      )
    )
  )

(setq memacs-img/before-export-aspect #'memacs-img//copy-to-destination-statics-dir)

(defun memacs-img--custom-link-img-export (path desc format)
  "export event of custom link img."
  ;; concat custom img host and final two section of the path
  (let ((url (funcall memacs-img/before-export-aspect path desc format)))
    (cond
     ((eq format 'html)
      (format "<img src=\"%s\" alt=\"%s\"/>" (url-encode-url url) desc))
     ((eq format 'md)
      (format "![%s](%s)" desc (url-encode-url url)))
     )
    )
  )


;;;; Toggle Inline Image

;; Codes is modified from 'org.el'
;; display-inline-images will display images whoes line type is 'file' or 'img'
;;;###autoload
(defun memacs-img-toggle-inline-images (&optional include-linked)
  "Toggle the display of inline images.
INCLUDE-LINKED is passed to `memacs-img--display-inline-images'."
  (interactive "P")
  (if org-inline-image-overlays
      (progn
        (org-remove-inline-images)
        (when (called-interactively-p 'interactive)
          (message "Inline image display turned off")))
    (memacs-img--display-inline-images include-linked)
    (when (called-interactively-p 'interactive)
      (message (if org-inline-image-overlays
                   (format "%d images displayed inline"
                           (length org-inline-image-overlays))
                 "No images to display inline")))))

(defun memacs-img--display-inline-images (&optional include-linked refresh beg end)
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


;;;; Init

(defun memacs-img--link-init ()
  "Add link type 'img'."
  (org-add-link-type "img" 'memacs-img--custom-link-img-follow 'memacs-img--custom-link-img-export)
  (message "Add Custom Link 'img'.")
  )

(memacs-img--link-init)

(provide 'memacs-org-ext)

;;; memacs-org-ext.el ends here
