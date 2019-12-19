;;; packages.el --- Language Server Protocol Layer packages file for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defconst lsp-packages
  '(
    (lsp-mode :requires yasnippet)
    lsp-ui
    (company-lsp :requires company)
    lsp-treemacs
    popwin
    ))

(defun lsp/init-lsp-mode ()
  (use-package lsp-mode
    :defer t
    :init
    (progn
      (setq lsp-clients-python-library-directories '("/usr/local/" "/usr/"))
      (setq lsp-clients-go-language-server-flags
            '("-gocodecompletion" "--format-style=\"goimports\"")))
    :config
    (progn
      (require 'lsp-clients)
      (spacemacs|diminish lsp-mode " Ⓛ" " L")
      (setq lsp-prefer-flymake nil)
      (spacemacs/lsp-bind-keys)
      (add-hook 'lsp-after-open-hook (lambda ()
                                       "Setup xref jump handler and declare keybinding prefixes"
                                       (spacemacs//setup-lsp-jump-handler major-mode)
                                       (spacemacs//lsp-declare-prefixes-for-mode major-mode)))
      )))

(defun lsp/init-lsp-ui ()
  (use-package lsp-ui
    :defer t
    :commands lsp-ui-mode
    :init (progn
            (add-hook 'lsp-mode-hook #'lsp-ui-mode))
    :config
    (progn
      (add-hook 'lsp-mode-hook #'memacs/lsp-mode-hook-func)

      (if lsp-remap-xref-keybindings
          (progn (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
                 (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)))

      (spacemacs/lsp-define-key
       lsp-ui-peek-mode-map
       "h" #'lsp-ui-peek--select-prev-file
       "j" #'lsp-ui-peek--select-next
       "k" #'lsp-ui-peek--select-prev
       "l" #'lsp-ui-peek--select-next-file
       )

      (setq memacs--lsp-ui-doc-max-width 1200)

      ;; redefine it to fix error position bug
      (defun lsp-ui-doc--move-frame (frame)
        "Place our FRAME on screen."
        (-let* (((left top _right _bottom) (window-edges nil nil nil t))
                (window (frame-root-window frame))
                ((width . height) (window-text-pixel-size window nil nil 10000 10000 t))
                (width (+ width (* (frame-char-width frame) 1))) ;; margins
                (width (if (< memacs--lsp-ui-doc-max-width width)
                           memacs--lsp-ui-doc-max-width
                         width))
                (char-h (frame-char-height))
                (height (min (- (* lsp-ui-doc-max-height char-h) (/ char-h 2)) height))
                (frame-resize-pixelwise t))
          (set-frame-size frame width height t)
          (set-frame-position
           frame
           (if (and (>= left (+ width 10 (frame-char-width)))
                  (not (lsp-ui-doc--next-to-side-window-p)))
               10
             (- (frame-pixel-width) width 10 (frame-char-width)))
           (pcase lsp-ui-doc-position
             ('top (+ top 10))
             ('bottom (- (lsp-ui-doc--line-height 'mode-line)
                         height
                         10))))))
      )))

(defun lsp/init-company-lsp ()
  (use-package company-lsp :defer t))

(defun lsp/init-lsp-treemacs ()
  (use-package lsp-treemacs :defer t))

(defun lsp/post-init-popwin ()
  (push '("*lsp-help*" :dedicated t :position bottom :stick t :noselect t :height 0.4)
        popwin:special-display-config))
