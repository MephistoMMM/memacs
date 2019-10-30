;;; mp-org/packages.el --- packages configurations for mp-org

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: spacemacs org elisp layer mp-org

;;; Commentary:

;;

;;; Code:


(defconst mp-org-packages
  '(
    org
    auctex
    (blog-admin :location (recipe
                           :fetcher github
                           :repo "MephistoMMM/blog-admin"))
    (advance-words-count :location (recipe
                                    :fetcher github
                                    :repo "MephistoMMM/advance-words-count.el"))
    ;; (maple-preview :location (recipe
    ;;                           :fetcher github
    ;;                           :repo "honmaple/emacs-maple-preview"))

    (memacs-org-ext :location local)
    org-wild-notifier

    ;; https://coldnew.github.io/d2d60fe2/
    pangu-spacing
    ;; (company-orz :location
    ;;              "~/.emacs.d/private/local/company-orz/")
    )
  )

;; (defun mp-org/init-maple-preview()
;;   "Init maple-preview
;; https://github.com/honmaple/emacs-maple-preview"
;;   (use-package maple-preview
;;     :defer t
;;     :commands (maple-preview-mode maple-preview-cleanup)
;;     :init
;;     (progn
;;       (setq
;;        maple-preview:home-path
;;        (expand-file-name (concat spacemacs-start-directory "extensions/maple-preview/"))
;;        maple-preview:preview-file
;;        (concat maple-preview:home-path "index.html")
;;        maple-preview:css-file
;;        (concat maple-preview:home-path "static/css/markdown.css")
;;        maple-preview:js-file nil))
;;     )
;;   )

(defun mp-org/init-org-wild-notifier()
  "A package which adds notification support for org-agenda views. With this
package you’ll get notifications for TODO entries.

 https://github.com/akhramov/org-wild-notifier.el"
  (use-package org-wild-notifier
    :after org
    :commands (org-wild-notifier-mode)
    :init
    (progn
      (setq alert-default-style 'osx-notifier))
    :config
    (progn
      (org-wild-notifier-mode)))
  )

(defun mp-org/init-pangu-spacing()
  "Insert White Spece Automatically."
  (use-package pangu-spacing
    :defer t)
  )

(defun mp-org/post-init-auctex ()
  "Use linenumber in LaTeX mode"
    (add-hook 'LaTeX-mode-hook 'spacemacs/toggle-line-numbers-on)
    ;; latex for perform full-document previews
    (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  )

(defun mp-org/init-advance-words-count()
  "Load advance-words-count"
  (use-package advance-words-count
    :defer t)
  )

(defun mp-org/init-memacs-org-ext ()
  "Init memacs-org-ext"
  (use-package memacs-org-ext
    :after org
    :commands (memacs/org-export-dispatch memacs-img-toggle-inline-images)
    :init
    (spacemacs/set-leader-keys-for-major-mode 'org-mode
      "ee" 'memacs/org-export-dispatch)
    :config
    (progn
      (setq mequ-conf-file "~/Dropbox/dotconf/mequ.conf")
      (setq custom-link-img-export-host
            "http://qiniu.oeli.pub")
      (when (configuration-layer/package-used-p 'ox-gfm)
        (memacs||export-org-in-dired "markdown" org-gfm-export-to-markdown))
      ))
  )

(defun mp-org/post-init-org ()
  "Configurations for org mode"
  (setq-default org-directory (expand-file-name "~/Dropbox/org"))

  (add-hook 'org-mode-hook (lambda ()
                             (if org-descriptive-links
                                 (progn (org-remove-from-invisibility-spec '(org-link))
                                        (org-restart-font-lock)
                                        (setq org-descriptive-links nil)))
                             (setq truncate-lines nil)) 'append)
  (setq-default org-bullets-bullet-list '("❁" "✾" "❀" "❖" "✧"))
  (with-eval-after-load 'org

    ;; config org-download and define custom link
    (setq-default
     org-download-image-dir (concat org-directory "/statics/")
     org-download-image-html-width 600
     org-download-link-format "[[img:%s]]"
     org-download-heading-lvl nil)

    (load-library "find-lisp")
    (setq-default
     org-startup-with-inline-images nil
     org-default-notes-file (concat org-directory "/TODOs.org"))
    (push (concat org-directory "/TODOs.org") org-agenda-files)

    (setq-default
     ;; org-log-done 'note
     org-agenda-span 'week
     org-agenda-restore-windows-after-quit t
     org-agenda-window-setup 'current-window
     org-footnote-auto-adjust t
     org-footnote-auto-label 'confirm)

    (setq-default
     ;; html export
     org-html-style-default ""
     org-html-htmlize-output-type nil
     org-todo-keywords
     '((sequence "TODO(t)" "WAITING(w@/!)" "|" "SHELVING(s@)" "DONE(d!)" "CANCELED(c@)")))
    )
  )


(defun mp-org/init-blog-admin ()
  "Configurations for blog-admin."
  (use-package blog-admin
    :defer t
    :commands blog-admin-start
    :init
    (progn
      (setq blog-admin-backend-path "~/Documents/Person/Blog")
      (setq blog-admin-backend-type 'hexo)
      (setq blog-admin-backend-new-post-in-drafts t) ;; create new post in drafts by default
      (setq blog-admin-backend-new-post-with-same-name-dir t) ;; create same-name directory with new post
      (setq blog-admin-backend-hexo-config-file "static_blog_config.yml") ;; default assumes _config.yml

      (add-hook 'blog-admin-backend-after-new-post-hook 'find-file)
      ))
   )

