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
  (let ((dest (read-directory-name "Export to Directory: "
                                   nil default-directory nil)))
    (setq memacs--org-export-directory (if (string-suffix-p "/" dest)
                                           dest (concat dest "/")))
    (org-export-dispatch))
  )

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
           )
       (setq memacs--org-export-directory dest)
       (mapc
        (lambda (f)
          (with-current-buffer
              (find-file-noselect f)
            (,org-export-func)))
        files))
     )
  )

;; export html

;;;###autoload (autoload 'memacs/export-org-in-dired-to-html "autoload/org" nil t)
(export-org-in-dired! "html" org-html-export-to-html)
