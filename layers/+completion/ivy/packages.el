;;; packages.el --- Ivy Layer packages File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(setq ivy-packages
      '(
        auto-highlight-symbol
        bookmark
        counsel
        counsel-projectile
        smex
        (hybrid-mode :location local)
        flx
        imenu
        ivy
        ivy-hydra
        (ivy-rich :toggle ivy-enable-advanced-buffer-information)
        (ivy-spacemacs-help :location local)
        (default-ivy-config :location built-in)
        org
        persp-mode
        projectile
        recentf
        swiper
        wgrep
        ivy-xref

        ;; (fuz :location built-in)
        ))

(defun ivy/pre-init-auto-highlight-symbol ()
  (spacemacs|use-package-add-hook auto-highlight-symbol
    :post-init
    ;; add some functions to ahs transient states
    (setq spacemacs--symbol-highlight-transient-state-doc
          (concat
           spacemacs--symbol-highlight-transient-state-doc
           "  Search: [_s_] swiper  [_b_] buffers  [_f_] files  [_/_] project"))
    (spacemacs/transient-state-register-add-bindings 'symbol-highlight
      '(("s" spacemacs/swiper-region-or-symbol :exit t)
        ("b" spacemacs/swiper-all-region-or-symbol :exit t)
        ("f" spacemacs/search-auto-region-or-symbol :exit t)
        ("/" spacemacs/search-project-auto-region-or-symbol :exit t)))))

(defun ivy/post-init-bookmark ()
  (spacemacs/set-leader-keys "fb" 'counsel-bookmark))

(defun ivy/init-counsel ()
  (use-package counsel
    :init
    (progn
      (spacemacs/set-leader-keys
        dotspacemacs-emacs-command-key 'counsel-M-x
        ;; files
        "ff"  'counsel-find-file
        "fel" 'counsel-find-library
        ;; help
        "?"   'counsel-descbinds
        "hda" 'counsel-apropos
        "hdf" 'counsel-describe-function
        "hdF" 'counsel-describe-face
        "hdm" 'spacemacs/describe-mode
        "hdv" 'counsel-describe-variable
        "hi"  'counsel-info-lookup-symbol
        ;; jump
        "fj" 'counsel-file-jump
        "jd" 'counsel-dired-jump
        "md" 'counsel-dired-jump
        ;; register/ring
        "rm"  'counsel-mark-ring
        ;; jumping
        "bj"  'spacemacs/counsel-jump-in-buffer
        ;; themes
        "Ts"  'counsel-load-theme
        )
      ;; search
      (memacs/define-search-keybinding
       "/" 'spacemacs/search-project-auto
       "?" 'spacemacs/search-project-auto-region-or-symbol
       "d" 'spacemacs/search-dir-auto
       "D" 'spacemacs/search-dir-auto-region-or-symbol
       "f" 'spacemacs/search-auto
       "F" 'spacemacs/search-auto-region-or-symbol
       )
      ;; ag search
      (memacs/declare-prefix-for-special-leader-key
        memacs--search-map-keybinding-prefix "a"
        "ag" "Search By Ag")
      (memacs/define-search-keybinding
       "a/" 'spacemacs/search-project-ag
       "a?" 'spacemacs/search-project-auto-region-or-ag
       "ad" 'spacemacs/search-dir-ag
       "aD" 'spacemacs/search-dir-ag-region-or-symbol
       "af" 'spacemacs/search-ag
       "aF" 'spacemacs/search-ag-region-or-symbol
       )
      ;; insert
      (memacs/define-insert-keybinding
       "c"  'counsel-unicode-char)
      )
    :config
    (progn
      ;; set additional ivy actions
      (ivy-set-actions
       'counsel-find-file
       memacs--ivy-file-actions)

      (define-key counsel-find-file-map (kbd "C-h") 'counsel-up-directory)
      (define-key read-expression-map (kbd "C-r") 'counsel-minibuffer-history)
      ;; remaps built-in commands that have a counsel replacement
      (counsel-mode 1)
      (spacemacs|hide-lighter counsel-mode)
      ;; TODO Commands to port
      (spacemacs//ivy-command-not-implemented-yet "jI")
      ;; Set syntax highlighting for counsel search results
      (ivy-set-display-transformer 'spacemacs/counsel-search 'counsel-git-grep-transformer))))

(defun ivy/pre-init-counsel-projectile ()
  ;; overwrite projectile settings
  (spacemacs|use-package-add-hook projectile
    :post-init
    (progn
      (setq projectile-switch-project-action 'counsel-projectile-find-file)

      (ivy-set-actions
       'counsel-projectile-find-file
       memacs--ivy-projectile-actions)
      ;; (append spacemacs--ivy-file-actions
      ;;         '(("R" (lambda (arg)
      ;;                  (interactive)
      ;;                  (call-interactively
      ;;                   #'projectile-invalidate-cache)
      ;;                  (ivy-resume)) "refresh list")
      ;;           ))

      (spacemacs/set-leader-keys
        "p SPC" 'counsel-projectile
        "pb"    'counsel-projectile-switch-to-buffer
        "pd"    'counsel-projectile-find-dir
        "pp"    'counsel-projectile-switch-project
        "pf"    'counsel-projectile-find-file))))

(defun ivy/post-init-hybrid-mode ()
  ;; registers
  (with-eval-after-load 'counsel
    (memacs/define-evil-keybinding
     (list
      evil-ex-completion-map  ;; TODO: replace command lack action after C-e or C-y
      evil-ex-search-keymap
      evil-normal-state-map
      evil-hybrid-state-map)
     "M-e" 'spacemacs/ivy-evil-registers
     "M-y" 'counsel-yank-pop))
  )

(defun ivy/init-flx ()
  (use-package flx))

(defun ivy/post-init-imenu ()
  (spacemacs/set-leader-keys "bi" 'spacemacs/counsel-jump-in-buffer))

(defun ivy/init-ivy ()
  (use-package ivy
    :init
    ;; Let core use ivy-completing-read
    (setq configuration-layer-completing-read-func 'ivy-completing-read)
    (progn
      ;; Key bindings
      (spacemacs/set-leader-keys
        "a'" 'spacemacs/ivy-available-repls
        "fr" 'counsel-recentf
        "bb" 'ivy-switch-buffer)
      (memacs/define-evil-normal-keybinding "M-u" 'ivy-resume))

    :config
    (progn
      ;; custom actions for recentf
      (ivy-set-actions
       'counsel-recentf
       memacs--ivy-file-actions)

      ;; add spacemacs/counsel-search command to ivy-highlight-grep-commands
      (add-to-list 'ivy-highlight-grep-commands 'spacemacs/counsel-search)

      ;; mappings to quit minibuffer or enter transient state
      (define-key ivy-minibuffer-map [escape] 'minibuffer-keyboard-quit)
      (define-key ivy-minibuffer-map (kbd "M-SPC") 'hydra-ivy/body)

      (when ivy-ret-visits-directory
        (define-key ivy-minibuffer-map (kbd "RET") #'ivy-alt-done)
        (define-key ivy-minibuffer-map (kbd "C-j") #'ivy-done))

      (ivy-mode 1)
      ;; Occur
      (evil-set-initial-state 'ivy-occur-grep-mode 'normal)
      (evil-make-overriding-map ivy-occur-mode-map 'normal)
      (ivy-set-occur 'spacemacs/counsel-search
                     'spacemacs//counsel-occur)
      (spacemacs/set-leader-keys-for-major-mode 'ivy-occur-grep-mode
        "w" 'spacemacs/ivy-wgrep-change-to-wgrep-mode
        "s" 'wgrep-save-all-buffers)
      ;; Why do we do this ?
      (ido-mode -1)

      ;; allow to select prompt in some ivy functions
      (setq ivy-use-selectable-prompt t)

      ;; see http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/
      ;; (add-hook 'minibuffer-setup-hook #'memacs-infinite-gc-cons)
      ;; (add-hook 'minibuffer-exit-hook #'memacs-origin-gc-cons)
      )))

(defun ivy/init-default-ivy-config ()
  (with-eval-after-load 'ivy
    (setq ivy-height 15
          ivy-re-builders-alist '((spacemacs/counsel-search . spacemacs/ivy--regex-plus)
                                  (t . ivy--regex-ignore-order)))
    (spacemacs|hide-lighter ivy-mode)
    ;; key bindings
    ;; ensure that the correct bindings are set at startup
    ;; (define-key ivy-minibuffer-map (kbd "C-n") 'ivy-next-line)
    ;; (define-key ivy-minibuffer-map (kbd "C-p") 'ivy-previous-line)
    ;; (define-key ivy-minibuffer-map (kbd "C-h") (kbd "DEL"))
    (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-kill-line)
    (define-key ivy-minibuffer-map (kbd "C-l") 'ivy-avy)
    (define-key ivy-minibuffer-map (kbd "<escape>") 'minibuffer-keyboard-quit)
    ;; Transient state
    ;; ivy-hydra disabled for now, waiting to see how the dependency management
    ;; evolves upstream
    ;; (require 'ivy-hydra)
    (spacemacs|define-transient-state ivy
      :doc "
 Move/Resize^^^^      | Select Action^^^^   |  Call^^          |  Cancel^^    | Toggles
--^-^-^-^-------------|--^-^-^-^------------|--^---^-----------|--^-^---------|---------------------
 [_j_/_k_] by line    | [_s_/_w_] next/prev | [_RET_] & done   | [_i_] & ins  | [_C_] calling: %s(if ivy-calling \"on\" \"off\")
 [_g_/_G_] first/last | [_a_]  ^ ^list all  | [_TAB_] alt done | [_q_] & quit | [_m_] matcher: %s(spacemacs//ivy-matcher-desc)
 [_d_/_u_] pg down/up |  ^ ^ ^ ^            | [_c_]   & cont   |  ^ ^         | [_f_] case-fold: %`ivy-case-fold-search
 [_<_/_>_] resize     |  ^ ^ ^ ^            | [_o_]   occur    |  ^ ^         | [_t_] truncate: %`truncate-lines
 [_h_/_l_] out/in dir | [_M-o_]^ ^do        |  ^ ^             |  ^ ^         |  ^ ^

Current Action: %s(ivy-action-name)
"
      :foreign-keys run
      :bindings
      ;; arrows
      ("j" ivy-next-line)
      ("k" ivy-previous-line)
      ("l" ivy-alt-done)
      ("h" spacemacs/counsel-up-directory-no-error)
      ("g" ivy-beginning-of-buffer)
      ("G" ivy-end-of-buffer)
      ("d" ivy-scroll-up-command)
      ("u" ivy-scroll-down-command)
      ;; actions
      ("q" keyboard-escape-quit :exit t)
      ("C-g" keyboard-escape-quit :exit t)
      ("<escape>" keyboard-escape-quit :exit t)
      ("i" nil)
      ("C-o" nil)
      ("M-SPC" nil)
      ("TAB" ivy-alt-done :exit nil)
      ("RET" ivy-done :exit t)
      ("c" ivy-call)
      ("C-m" ivy-done :exit t)
      ("C" ivy-toggle-calling)
      ("m" ivy-rotate-preferred-builders)
      (">" ivy-minibuffer-grow)
      ("<" ivy-minibuffer-shrink)
      ("w" ivy-prev-action :exit t)
      ("s" ivy-next-action :exit t)
      ("a" ivy-read-action :exit t)
      ("M-o" ivy-dispatching-done-hydra :exit t)
      ("t" (setq truncate-lines (not truncate-lines)))
      ("f" ivy-toggle-case-fold)
      ("o" ivy-occur :exit t))
    (define-key ivy-minibuffer-map "\C-o" 'spacemacs/ivy-transient-state/body)
    )

  ;; binding yank pop and evil-registers
  (with-eval-after-load 'counsel
    (define-key ivy-minibuffer-map (kbd "M-e") 'spacemacs/ivy-evil-registers)
    (define-key ivy-minibuffer-map (kbd "M-y") 'counsel-yank-pop))
  )

(defun ivy/init-ivy-hydra ()
  (use-package ivy-hydra)
  (define-key hydra-ivy/keymap [escape] 'hydra-ivy/keyboard-escape-quit-and-exit))

(defun ivy/init-ivy-rich ()
  ;; More friendly display transformer for Ivy
  (use-package ivy-rich
    :after counsel
    :defines (all-the-icons-mode-icon-alist all-the-icons-dir-icon-alist bookmark-alist)
    :functions (all-the-icons-icon-family
                all-the-icons-match-to-alist
                all-the-icons-auto-mode-match?
                all-the-icons-octicon
                all-the-icons-dir-is-submodule)
    :hook (ivy-rich-mode . (lambda ()
                             (setq ivy-virtual-abbreviate
                                   (or (and ivy-rich-mode 'abbreviate) 'name))))
    :preface
    (with-eval-after-load 'all-the-icons
      (add-to-list 'all-the-icons-mode-icon-alist
                   '(gfm-mode  all-the-icons-octicon "markdown" :v-adjust 0.0 :face all-the-icons-lblue)))
    :config
    (progn
      (setq ivy-rich-parse-remote-buffer nil)
      (ivy-rich-mode 1)))
  )


(defun ivy/init-ivy-spacemacs-help ()
  (use-package ivy-spacemacs-help
    :commands (ivy-spacemacs-help-dotspacemacs
               ivy-spacemacs-help
               ivy-spacemacs-help-layers
               ivy-spacemacs-help-packages
               ivy-spacemacs-help-toggles)
    :init (spacemacs/set-leader-keys
            "h ."   'ivy-spacemacs-help-dotspacemacs
            "h SPC" 'ivy-spacemacs-help
            "h l"   'ivy-spacemacs-help-layers
            "h p"   'ivy-spacemacs-help-packages
            "h t"   'ivy-spacemacs-help-toggles)))

(defun ivy/post-init-org ()
  (add-hook 'org-ctrl-c-ctrl-c-hook 'spacemacs//counsel-org-ctrl-c-ctrl-c-org-tag))

(defun ivy/pre-init-persp-mode ()
  (spacemacs|use-package-add-hook persp-mode
    :post-config
    (setq
     spacemacs--persp-display-buffers-func 'spacemacs/ivy-spacemacs-layout-buffer
     spacemacs--persp-display-perspectives-func 'spacemacs/ivy-spacemacs-layouts)))

(defun ivy/post-init-persp-mode ()
  ;; based on https://gist.github.com/Bad-ptr/1aca1ec54c3bdb2ee80996eb2b68ad2d#file-persp-ivy-el
  (add-hook 'ivy-ignore-buffers #'spacemacs//layout-not-contains-buffer-p)
  (setq ivy-sort-functions-alist
        (append ivy-sort-functions-alist
                '((persp-kill-buffer . nil)
                  (persp-remove-buffer . nil)
                  (persp-add-buffer . nil)
                  (persp-switch . nil)
                  (persp-window-switch . nil)
                  (persp-frame-switch . nil))))

  (ivy-set-actions
   'spacemacs/ivy-spacemacs-layouts
   '(("c" persp-kill-without-buffers "Close layout(s)")
     ("k" persp-kill  "Kill layout(s)")
     ("n" persp-copy "Copy Current Layout")
     ("p" spacemacs//create-persp-with-current-project-buffers
      "Create Project Layout")))
  ;; TODO: better handling of C and X bindings for ivy
  ;;       check ivy/pre-init-persp-mode
  (spacemacs/transient-state-register-remove-bindings 'layouts
    '("C" "X"))
  (spacemacs/transient-state-register-add-bindings 'layouts
    '(("C" spacemacs/ivy-spacemacs-layout-close-other :exit t)
      ("X" spacemacs/ivy-spacemacs-layout-kill-other :exit t))))

(defun ivy/post-init-projectile ()
  (setq projectile-completion-system 'ivy)
  (spacemacs/set-leader-keys
    "pv"  'projectile-vc))

(defun ivy/post-init-recentf ()
  ;; custom actions for recentf
  (ivy-set-actions
   'counsel-recentf
   (append memacs--ivy-file-actions
           '(("F" (lambda (arg)
                    (interactive)
                    (recentf-cleanup)
                    (ivy-recentf)) "refresh list")
             ("D" (lambda (arg)
                    (interactive)
                    (setq recentf-list (delete arg recentf-list))
                    (ivy-recentf)) "delete from list"))))
  ;; merge recentf and bookmarks into buffer switching. If we set this
  (setq ivy-use-virtual-buffers t))

(defun ivy/init-swiper ()
  (use-package swiper
    :config
    (progn
      (memacs/define-search-keybinding
       "S" 'spacemacs/swiper-region-or-symbol
       "b" 'swiper-all
       "B" 'spacemacs/swiper-all-region-or-symbol)
      (define-key swiper-map (kbd "C-l") 'swiper-avy)
      )))

(defun ivy/init-smex ()
  (use-package smex
    :defer t
    :init (setq-default smex-history-length 32
                        smex-save-file (concat spacemacs-cache-directory
                                               ".smex-items"))))

(defun ivy/init-wgrep ()
  (evil-define-key 'normal wgrep-mode-map ",," 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",c" 'wgrep-finish-edit)
  (evil-define-key 'normal wgrep-mode-map ",a" 'wgrep-abort-changes)
  (evil-define-key 'normal wgrep-mode-map ",k" 'wgrep-abort-changes))


;; used for lsp
(defun ivy/init-ivy-xref ()
  (use-package ivy-xref
    :defer t
    :init
    (progn
      (setq xref-prompt-for-identifier '(not xref-find-definitions
                                            xref-find-definitions-other-window
                                            xref-find-definitions-other-frame
                                            xref-find-references
                                            spacemacs/jump-to-definition))

      ;; Use ivy-xref to display `xref.el' results.
      (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))))

(defun ivy/init-fuz ()
  "fuz.el provides some fuzzy match scoring/matching functions for Emacs, they are powered by Rust so it’s fast enough."
  (let ((absolute-fuz-load-path memacs-fuz-load-path))
    (when (file-directory-p absolute-fuz-load-path)
      (add-to-load-path absolute-fuz-load-path)
      (use-package ivy-fuz
        :after ivy
        :init
        (progn
          (setq ivy-sort-matches-functions-alist '((t . ivy-fuz-sort-fn)))
          (setq ivy-re-builders-alist '((t . ivy-fuz-regex-fuzzy))))
        :config
        (add-to-list 'ivy-highlight-functions-alist '(ivy-fuz-regex-fuzzy . ivy-fuz-highlight-fn))))
    )
  )
