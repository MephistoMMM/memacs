;;; memacs/autoload.el -*- lexical-binding: t; -*-

;; mission
(defvar memacs-mission-starter-mission-list
  '(
    ;; config new org file in dropbox
    ("Dropbox Note Buffer" org-mode (concat org-directory "/notes") nil)
    )
  "Mission list. Format (buffer-name mode path file-name). if file-name is non-nil, it will set vistied file name.")

(defvar memacs-mission-helper-help-list
  '(
    ("Projectile" ("ignoring files" "https://projectile.readthedocs.io/en/latest/usage/#ignoring-files"))
    )
  "Help list. Format (help-item-name ((help-subitem-name url))).")

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

;;;###autoload
(defun +memacs/mission-starter-start(mission)
  "Select a mission to start from memacs-mission-start-mission-list."
  (interactive
   (list (ivy-completing-read "MISSIONS:"
                              #'memacs//mission-start-candidates-function nil t)))
  (let ((mode (nth 0 (get-text-property 0 'property mission)))
        (path (nth 1 (get-text-property 0 'property mission)))
        (file (nth 2 (get-text-property 0 'property mission)))
        (bak--default-directory default-directory))
    (setq default-directory (if (stringp path) path (eval path)))
    (when (or (stringp file) (listp file))
      (let ((visited-file-name (if (stringp file)
                                   (concat default-directory "/" file)
                                 (eval file))))
        (setq default-directory bak--default-directory)
        (find-file visited-file-name))
      ))
  )

(defun memacs//mission-help-candidates-function (str pred _)
  (mapcar (lambda (help)
            (propertize (car help) 'subhelp (cdr help)))
          memacs-mission-helper-help-list))

;;;###autoload
(defun +memacs/mission-helper-help(help)
  "Select a mission to start from memacs-mission-start-mission-list."
  (interactive
   ;; TODO write function support choose any level menu tree
   (list (let* ((top-item (ivy-completing-read "HELP LIST:"
                                               #'memacs//mission-help-candidates-function
                                               nil t))
                (subhelp (get-text-property 0 'subhelp top-item)))
           (if (listp (car subhelp))
               ;; two level help menu
               (ivy-completing-read "SUBHELP LIST:"
                                    (lambda (str pred _)
                                      (mapcar (lambda (shp)
                                                (propertize
                                                 (car shp)
                                                 'url (cdr shp)))
                                              subhelp))
                                    nil t)
             ;; single level
             (propertize top-item 'url subhelp))
           )))
  (let ((url (nth 0 (get-text-property 0 'url help))))
    (if url
        (browse-url url browse-url-new-window-flag)
      (error "No URL found"))
    )
  )

;; org export
(defvar memacs--org-export-directory (expand-file-name "~/Desktop"))

;;;###autoload
(defun +memacs-org-export-dispatch (&optional arg)
  "Change exported destination to the special path."
  (interactive "P")
  ;; let org-attach init
  (unless (boundp 'org-attach-expand-links)
    (org-toggle-inline-images))
  (let ((dest (read-directory-name "Export to Directory: "
                                   nil default-directory nil)))
    (let ((memacs-org-export-attachment-filepath-a #'memacs--copy-to-destination-statics-dir)
          (memacs--org-export-directory (if (string-suffix-p "/" dest)
                                            dest (concat dest "/"))))
      (org-export-dispatch)))
  )

(defvar memacs-org-export-attachment-filepath-a nil
  "Point cut for getting the file path of attachment.")

;; HACK rewrite function in org-mode
(defun memacs-org-attach-expand-links (_)
  "Expand links in current buffer.
It is meant to be added to `org-export-before-parsing-hook'."
  (doom-log "run org-attach-expand-links with %s" memacs-org-export-attachment-filepath-a)
  (save-excursion
    (while (re-search-forward "attachment:" nil t)
      (let ((link (org-element-context)))
        (when (and (eq 'link (org-element-type link))
                   (string-equal "attachment"
                                 (org-element-property :type link)))
          (let* ((description (and (org-element-property :contents-begin link)
                                   (buffer-substring-no-properties
                                    (org-element-property :contents-begin link)
                                    (org-element-property :contents-end link))))
                 (file (org-element-property :path link))
                 (filepath (org-attach-expand file))
                 (new-link (org-link-make-string
                            (concat "file:"
                                    (if  memacs-org-export-attachment-filepath-a
                                        (funcall memacs-org-export-attachment-filepath-a file filepath)
                                      filepath))
                            description)))
            (goto-char (org-element-property :end link))
            (skip-chars-backward " \t")
            (delete-region (org-element-property :begin link) (point))
            (insert new-link)))))))

;; export files in dired or directory
(defmacro export-org-in-dired! (type org-export-func)
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
           (let ((memacs-org-export-attachment-filepath-a memacs--copy-to-destination-statics-dir)
                 (memacs--org-export-directory dest))
             (mapc
              (lambda (f)
                (with-current-buffer
                    (find-file-noselect f)
                  (,org-export-func)))
              files))
           ))
     )
  )

(defun memacs--copy-to-destination-statics-dir (filename filepath)
  "A aspect for copy src file to statics dir in destination."
  (doom-log "copy %s(%s) to statics" filepath filename)
  (let ((statics-dir (concat memacs--org-export-directory "statics/")))
    (if (not (file-exists-p filepath))
        ;; return nil if image does not exist
        nil
      ;; modify url and copy image file if image exists
      (unless (file-exists-p statics-dir)
        (make-directory statics-dir))
      (let ((dest (concat statics-dir filename))
            (relative-path (concat "./statics/" filename)))
        (unless (file-exists-p dest)
          (copy-file filepath dest))
        relative-path))))

(defun memacs--export-to-Internet (filename filepath)
  "A aspect for change path to url in the internet."
  (concat custom-link-attachment-export-host "/statics/" filename))

;; export html

;;;###autoload (autoload 'memacs/export-org-in-dired-to-html "autoload/org" nil t)
(export-org-in-dired! "html" org-html-export-to-html)

;; Roam
;; org-roam
(defvar memacs-org-roam-languages
  `(go
    python
    c
    shell
    java
    javascript
    web
    lua
    rust
    sql
    haskell
    latex
    applescript
    antlr
    lisp)
  "Avaliable language for org-roam-capture filename.")

(defvar memacs--org-roam-complete-software-directories nil)
(defvar memacs--org-roam-complete-program-languages nil)

;;;###autoload
(defun memacs-org-roam-complete-software-directories ()
  "Complete name of directories under `roam/software'."
  (interactive)
  (memacs--org-roam-complete-directories
    (expand-file-name "software" org-roam-directory)
    "Software Name: "
    'memacs--org-roam-complete-software-directories))

;;;###autoload
(defun memacs-org-roam-complete-program-languages ()
  "Complete name of languages."
  (interactive)
  (ivy-completing-read
    "Language Name: "
    (mapcar #'symbol-name memacs-org-roam-languages) nil nil
    (car memacs--org-roam-complete-program-languages)
    'memacs--org-roam-complete-program-languages))

(defun memacs--org-roam-complete-directories (parent-dir message &optional history-symbol)
  "Complete name of directories under `parent-dir'."
  (let ((history (or (symbol-value history-symbol) (list)))
        (conditions (unless (f-directory? parent-dir) (list-directory parent-dir))))
    (ivy-completing-read message
                         conditions nil nil
                         (car history)
                         history-symbol)))

;;;###autoload
(defun +org-export-parse-and-replace-tables ()
  "Parse org buffer and replace tables to latex format."
  (when (memacs-org-export-table-latex-converter-existp)
    (let ((tree (org-element-parse-buffer 'greater-element))
          (offset 0))
      (dolist (item (org-element-map tree 'table #'identity))
        (setq offset (+ offset
                        (memacs-org-export-replace-table-item
                         (- (org-element-property :begin item) offset)
                         (- (org-element-property :end item) offset)))
              ))))
  )

(defun memacs-org-export-replace-table-item (begin end)
  "Delete table and insert new content."
  (goto-char begin)
  (let ((new-content (memacs-org-export-table-latex-converter begin end)))
    (insert new-content)
    (- (- end begin) (length new-content)))
  )

(defun memacs-org-export-table-latex-converter-existp ()
  "Check if latex converter exits."
  (executable-find "gorg-util"))

(defun memacs-org-export-table-latex-converter (begin end)
  "The converter used to converter table content to latex format."
  (let ((result (call-process-region
                 begin end "gorg-util" t
                 (generate-new-buffer "*GORG_UTIL*") nil
                 "-i"))
        (content (with-current-buffer "*GORG_UTIL*"
                   (buffer-string))))
    (kill-buffer "*GORG_UTIL*")
    (if (= 0 result)
        content
      (message "Error to execute gorg-util: %s" content)
      "")
    ))

(defvar +memacs-convenient-frame-default-directory
  (expand-file-name "~/Desktop/")
  "Default directory of convenient frame's buffer.")

(defvar +memacs-convenient-frame-parameters
  `((name . "memacs-convenient-frame")
    (width . 100)
    (height . 25)
    (transient . t)
    ,@(when IS-LINUX
        `((window-system . ,(if (boundp 'pgtk-initialized) 'pgtk 'x))
          (display . ,(or (getenv "DISPLAY") ":0"))))
    ,(if IS-MAC '(menu-bar-lines . 1))))

;;;###autoload
(defun +memacs/open-convenient-frame (&optional initial-input)
  "Opens the window in a floating frame that cleans itself up once
you're done. This can be called from an external shell script."
  (interactive)
  (when (and initial-input (string-empty-p initial-input))
    (setq initial-input nil))
  (let* ((frame-title-format "")
         (frame (make-frame +memacs-convenient-frame-parameters)))
    (select-frame-set-input-focus frame)  ; fix MacOS not focusing new frames
    (with-selected-frame frame
      (condition-case ex
          (let ((buffer (generate-new-buffer "*MEMACS-CONVENIENT*")))
            (set-window-buffer nil buffer)
            (with-current-buffer buffer
              (setq-local default-directory
                          +memacs-convenient-frame-default-directory)
              (org-mode)
              (when (stringp initial-input)
                (remove-text-properties 0 (length initial-input) '(read-only t) initial-input)
                (insert initial-input))
              ))
        ('error
         (message "memacs-convenient: %s" (error-message-string ex))
         (delete-frame frame))))))
