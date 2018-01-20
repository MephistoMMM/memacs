;;; mp-base/funcs.el --- defines functions for base

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: base functions spacemacs elisp

;;; Commentary:

;; description

;;; Code:

;;;; Better-Default

(defun mp-base/spaceline-compile ()
  "Spaceline Compile Using Myself Theme"
  (spaceline-compile
          `(((major-mode buffer-modified)
                :fallback evil-state
                :face highlight-face)
              auto-compile
              ((persp-name workspace-number window-number)
                :separator "|")
              (process :when active)
              (minor-modes :when active)
              (buffer-size  remote-host buffer-id)
              (mu4e-alert-segment :when active)
              (erc-track :when active)
              (version-control :when active)
              (org-pomodoro :when active)
              (org-clock :when active))

          `(which-function
            (python-pyvenv :fallback python-pyenv)
            (battery :when active)
            anzu
            selection-info
            input-method
            ((buffer-encoding-abbrev
              point-position
              line-column)
              :separator " | ")
            ((flycheck-error flycheck-warning flycheck-info)
              :when active)
            (global :when active)
            buffer-position
            hud))
    (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))
  )
(defun spacemacs/get-mode-line-theme-name ()
  "Return the mode-line theme name."
  (if (listp dotspacemacs-mode-line-theme)
      (car dotspacemacs-mode-line-theme)
    dotspacemacs-mode-line-theme))

(defun spacemacs/mode-line-separator ()
  "Return the separator type for the mode-line.
Return nil if no separator is defined."
  (when (listp dotspacemacs-mode-line-theme)
    (plist-get (cdr dotspacemacs-mode-line-theme) :separator)))

(defun spacemacs/mode-line-separator-scale ()
  "Return the separator scale for the mode-line.
Return nil if no scale is defined."
  (when (listp dotspacemacs-mode-line-theme)
    (plist-get (cdr dotspacemacs-mode-line-theme) :separator-scale)))


;; spaceline

(defun spacemacs/customize-powerline-faces ()
  "Alter powerline face to make them work with more themes."
  (when (boundp 'powerline-inactive2)
    (set-face-attribute 'powerline-inactive2 nil
                        :inherit 'font-lock-comment-face)))

(defun spacemacs//evil-state-face ()
  (let ((state (if (eq 'operator evil-state) evil-previous-state evil-state)))
    (intern (format "spacemacs-%S-face" state))))

(defun spacemacs//restore-powerline (buffer)
  "Restore the powerline in buffer"
  (with-current-buffer buffer
    (setq-local mode-line-format (default-value 'mode-line-format))
    (powerline-set-selected-window)
    (powerline-reset)))

(defun spacemacs//set-powerline-for-startup-buffers ()
  "Set the powerline for buffers created when Emacs starts."
  (dolist (buffer '("*Messages*" "*spacemacs*" "*Compile-Log*"))
    (when (and (get-buffer buffer)
               (configuration-layer/package-used-p 'spaceline))
      (spacemacs//restore-powerline buffer))))

(defun spacemacs//prepare-diminish ()
  (when spaceline-minor-modes-p
    (let ((unicodep (dotspacemacs|symbol-value
                     dotspacemacs-mode-line-unicode-symbols)))
      (setq spaceline-minor-modes-separator
            (if unicodep (if (display-graphic-p) "" " ") "|"))
      (dolist (mm spacemacs--diminished-minor-modes)
        (let ((mode (car mm)))
          (when (and (boundp mode) (symbol-value mode))
            (let* ((unicode (cadr mm))
                   (ascii (caddr mm))
                   (dim (if unicodep
                            unicode
                          (if ascii ascii unicode))))
              (diminish mode dim))))))))

;;; mp-base/funcs.el ends here
