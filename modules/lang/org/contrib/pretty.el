;;; lang/org/contrib/pretty.el -*- lexical-binding: t; -*-
;;;###if (modulep! +pretty)
(after! org
  (setq org-highlight-latex-and-related '(native script entities)))


(use-package! org-superstar ; "prettier" bullets
  :hook (org-mode . org-superstar-mode)
  :config
  ;; Make leading stars truly invisible, by rendering them as spaces!
  (setq org-superstar-leading-bullet ?\s
        org-superstar-leading-fallback ?\s
        org-hide-leading-stars nil

        ;; Don't do anything special for item bullets or TODOs by default; these slow
        ;; down larger org buffers.
        org-superstar-prettify-item-bullets nil
        org-superstar-special-todo-items nil
        ;; ...but configure it in case the user wants it later
        org-superstar-headline-bullets-list '("✾" "✧" "❀" "✿" "❖")
        org-superstar-todo-bullet-alist
        '(("TODO" . 9744)
          ("[ ]"  . 9744)
          ("DONE" . 9745)
          ("[X]"  . 9745))))


(use-package! org-fancy-priorities ; priority icons
  :hook (org-agenda-mode . org-fancy-priorities-mode)
  :config (setq org-fancy-priorities-list '("☢" "☕" "■")))
;; ("❗" "⬆" "⬇" "☕")
