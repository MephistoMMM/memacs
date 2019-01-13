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

    (memacs-img-link :location local)

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

(defun mp-org/init-memacs-img-link ()
  "Init memacs-img-link"
  (use-package memacs-img-link
    :after org
    :config
    (progn
      (setq mequ-conf-file "~/Dropbox/dotconf/mequ.conf")
      (setq custom-link-img-export-host
            "http://qiniu.oeli.pub")
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
  (with-eval-after-load 'org

    ;; config org-download and define custom link
    (setq-default
     org-download-image-dir (concat org-directory "/statics/")
     org-download-image-html-width 600
     org-download-link-format "[[img:%s]]"
     org-download-heading-lvl nil)

    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (http . t)
       (dot . t)
       (C . t)))

    (load-library "find-lisp")
    (setq-default
      org-startup-with-inline-images nil
      org-bullets-bullet-list '("❁" "✾" "❀" "❖" "✧")
      org-agenda-files (directory-files org-directory t "\.org$" t)
      org-default-notes-file (concat org-directory "/TODOs.org"))

    (setq-default
      ;; org-log-done 'note
      org-agenda-span 'day
      org-agenda-restore-windows-after-quit t
      org-agenda-window-setup 'other-window
      org-footnote-auto-adjust t
      org-footnote-auto-label 'confirm)

    (setq-default
      ;; html export
      org-html-style-default ""
      org-html-htmlize-output-type nil
      org-todo-keywords
      '((sequence "TODO(t)" "WAITING(w@/!)" "|" "SHELVING(s@)" "DONE(d!)" "CANCELED(c@)")))

    (setq-default
      org-modules
      '(org-bbdb org-habit org-info org-irc org-w3m org-mac-link org-protocol)
      org-capture-templates
      '(("w" "Task" entry
         (file+headline (lambda () (concat org-directory "/TODOs.org")) "Fighting")
         "* TODO [#A] %^{Task}\nSCHEDULED: %t\n")

        ("t" "Todo" entry
         (file+headline (lambda () (concat org-directory "/TODOs.org")) "Play Space")
         "* TODO [#%^{level|B|C}] %?\nSCHEDULED: %t\n%i\n"
         :empty-lines 1)

        ("l" "Links" entry
         (file+headline (lambda () (concat org-directory "/TODOs.org")) "Play Space")
         "* TODO [#C] %? link \t%^g\nCaptured On: %U\n"
         :empty-lines 1)

        ("b" "Books" entry
         (file+headline (lambda () (concat org-directory "/TODOs.org")) "Books")
         "* TODO [#B] %?"
         :empty-lines 1)

        ("n" "Temporary Notes" entry
         (file+headline (lambda () (concat org-directory "/Temp.org")) "Temporary Notes")
         "* %?\n  %i%a\n%U"
         :empty-lines 1)
      )
    ))
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

