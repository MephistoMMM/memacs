;;; core-themes-support.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defvar spacemacs--fallback-theme 'spacemacs-dark
  "Fallback theme if user theme cannot be applied.")

(defvar spacemacs--delayed-user-theme nil
  "Internal variable storing user theme to be installed.")

(defvar spacemacs--cur-theme nil
  "Internal variable storing currently loaded theme.")

(defface org-kbd
  '((t (:background "LemonChiffon1" :foreground "black" :box
                    (:line-width 2 :color nil :style released-button))))
  "Face for displaying key bindings in Spacemacs documents."
  :group 'org-faces)

(defvar spacemacs-theme-name-to-package
  '(
    (spacemacs-dark  . spacemacs-theme)
    (spacemacs-light . spacemacs-theme)
    )
  "alist matching a theme name with its package name, required when
package name does not match theme name + `-theme' suffix.")

(defvar spacemacs-post-theme-change-hook nil
  "Hook run after theme has changed.")

(defun spacemacs/get-theme-package-name (theme-name)
  "Returns the package theme for the given THEME name."
  (cond
   ;; from explicit alist
   ((assq theme-name spacemacs-theme-name-to-package)
    (cdr (assq theme-name spacemacs-theme-name-to-package)))
   ;; fallback to <name>-theme
   (t (intern (format "%S-theme" theme-name)))))

(defun spacemacs//get-theme-name (theme)
  "Return the name of THEME."
  (if (listp theme)
      (car theme)
    theme))

(defun spacemacs//get-theme-package-directory (theme)
  "Return the THEME location on disk."
  (let* ((theme-name (spacemacs//get-theme-name theme))
         (pkg-name (spacemacs/get-theme-package-name theme-name))
         (dir (when (listp theme)
                (configuration-layer/get-location-directory
                 pkg-name
                 (plist-get (cdr theme) :location)
                 'dotfile))))
    (unless dir
      ;; fallback to elpa directory
      (setq dir (configuration-layer/get-elpa-package-install-directory
                 pkg-name)))
    dir))

(defun spacemacs/load-default-theme (&optional fallback-theme disable)
  "Load default theme.
Default theme is the car of `dotspacemacs-themes'.
If FALLBACK-THEME is non-nil it must be a package name which will be loaded if
THEME cannot be applied."
  (spacemacs/load-theme (car dotspacemacs-themes) fallback-theme disable))

(defun memacs//default-theme-package-init-func (theme)
  "Default package init function,
 extracted from origin `spacemacs/load-theme'."
  (let ((pkg-dir (spacemacs//get-theme-package-directory theme))
        (pkg-name (spacemacs/get-theme-package-name theme-name)))
    (when pkg-dir
      ;; package activate should be enough, but not all themes
      ;; have add themselves to `custom-theme-load-path' in autoload.
      ;; (for example, moe-theme).
      (add-to-list 'custom-theme-load-path pkg-dir)
      (package-activate pkg-name)))
  )

(defun spacemacs/load-theme (theme &optional fallback-theme disable)
  "Apply user theme.
If FALLBACK-THEME is non-nil it must be a package name which will be loaded if
THEME cannot be applied.
If DISABLE is non-nil then disable all previously applied themes before applying
THEME."
  (let ((theme-name (spacemacs//get-theme-name theme)))
    (condition-case err
        (progn
          ;; Load theme
          (unless (or (memq theme-name (custom-available-themes))
                     (eq 'default theme-name))
            (let ((theme-args
                   (cdr (assq theme-name spacemacs-theme-name-to-package))))
              (cond
               ((functionp theme-args)
                (funcall theme-args theme))
               ((listp theme-args)
                (apply (car theme-args) (cdr theme-args)))
               (t (memacs//default-theme-package-init-func theme)))
              ))
          (when disable
            (mapc 'disable-theme custom-enabled-themes))
          (unless (eq 'default theme-name)
            (load-theme theme-name t))
          (unless (display-graphic-p)
            (eval `(spacemacs|do-after-display-system-init
                    (load-theme ',theme-name t))))
          (setq-default spacemacs--cur-theme theme-name))
      ('error
       (message "error: %s" err)
       (if fallback-theme
           ;; fallback to Spacemacs default theme
           (progn
             (setq spacemacs--delayed-user-theme theme-name)
             (spacemacs/load-fallback-theme fallback-theme disable))
         ;; no fallback theme was specified, so we log explicit warning
         (spacemacs-buffer/warning
          (concat "An error occurred while applying "
                  "the theme \"%s\", fallback on theme \"%s\". \n"
                  "Error was: %s")
          theme-name spacemacs--fallback-theme err)
         (spacemacs-buffer/warning
          (concat "Please check the value of \"dotspacemacs-themes\" in your "
                  "dotfile or open an issue \n"
                  "so we can add support for the theme \"%s\".")
          theme-name))))))

(defun spacemacs/load-fallback-theme (theme &optional disable)
  "Apply the fallback theme.
If DISABLE is non-nil then disable all previously applied themes before applying
THEME."
  (let ((theme-name (spacemacs//get-theme-name theme)))
    ;; pop up fallback theme to the top of the list
    (setq spacemacs--cur-theme theme-name)
    (setq dotspacemacs-themes (delq theme-name dotspacemacs-themes))
    (add-to-list 'dotspacemacs-themes theme-name)
    (when disable
      (mapc 'disable-theme custom-enabled-themes))
    (load-theme theme-name t)
    (unless (display-graphic-p)
      (eval `(spacemacs|do-after-display-system-init
              (load-theme ',theme-name t))))))

(defun spacemacs/cycle-spacemacs-theme (&optional backward)
  "Cycle through themes defined in `dotspacemacs-themes'.
When BACKWARD is non-nil, or with universal-argument, cycle backwards."
  (interactive "P")
  (let* ((themes (if backward (reverse dotspacemacs-themes) dotspacemacs-themes))
         (next-theme (car (or (cdr (memq spacemacs--cur-theme themes))
                              ;; if current theme isn't in cycleable themes, start
                              ;; over
                              themes))))
    (when spacemacs--cur-theme
      (disable-theme spacemacs--cur-theme))
    (let ((progress-reporter
           (make-progress-reporter
            (format "Loading theme %s..." next-theme))))
      (spacemacs/load-theme next-theme nil 'disable)
      (progress-reporter-done progress-reporter))))

(defun spacemacs/cycle-spacemacs-theme-backward ()
  "Cycle through themes defined in `dotspacemacs-themes' backward."
  (interactive)
  (spacemacs/cycle-spacemacs-theme t))

(defadvice load-theme (after spacemacs/load-theme-adv activate)
  "Perform post load processing."
  (let ((theme (ad-get-arg 0)))
    ;; Without this a popup is raised every time emacs25 starts up for
    ;; assignment to a free variable
    (with-no-warnings
      (setq spacemacs--cur-theme theme))
    (spacemacs/post-theme-init theme)))

(defun spacemacs/post-theme-init (theme)
  "Some processing that needs to be done when the current theme
has been changed to THEME."
  (interactive)
  (run-hooks 'spacemacs-post-theme-change-hook))

;; (defun spacemacs//add-theme-packages-to-additional-packages ()
;;   "Add all theme packages from `dotspacemacs-themes' to packages to install."
;;   (setq dotspacemacs--additional-theme-packages nil)
;;   (dolist (theme dotspacemacs-themes)
;;     (let* ((theme-name (spacemacs//get-theme-name theme))
;;            (pkg-name (spacemacs/get-theme-package-name theme-name))
;;            (theme2 (copy-tree theme)))
;;       (when pkg-name
;;         (if (listp theme2)
;;             (setcar theme2 pkg-name)
;;           (setq theme2 pkg-name))
;;         (add-to-list 'dotspacemacs--additional-theme-packages theme2)))))
;; (add-hook 'configuration-layer-pre-load-hook
;;           'spacemacs//add-theme-packages-to-additional-packages)

(provide 'core-themes-support)
