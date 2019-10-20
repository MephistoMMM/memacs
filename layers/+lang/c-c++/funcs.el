;;; funcs.el --- C/C++ Layer functions File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(require 'cl-lib)
(require 'subr-x)

(defun spacemacs//c-toggle-auto-newline ()
  "Toggle auto-newline."
  (c-toggle-auto-newline 1))

(defun memacs//c-mode-keybinding-modify ()
  (define-key c-mode-map (kbd ";") nil))


;; clang

(defun spacemacs/clang-format-function (&optional style)
  "Format the current function with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (c-mark-function)
    (clang-format (region-beginning) (region-end) style)
    (deactivate-mark) ; If the function is already formatted, then remove the mark
    (message "Formatted function %s" (c-defun-name))))

(defun spacemacs/clang-format-region-or-buffer (&optional style)
  "Format the current region or buffer with clang-format according to STYLE."
  (interactive)
  (save-excursion
    (if (region-active-p)
        (progn
          (clang-format-region (region-beginning) (region-end) style)
          (message "Formatted region"))
      (progn
        (clang-format-buffer style)
        (message "Formatted buffer %s" (buffer-name))))))

(defun spacemacs//clang-format-on-save ()
  "Format the current buffer with clang-format on save when
`c-c++-enable-clang-format-on-save' is non-nil."
  (when c-c++-enable-clang-format-on-save
    (spacemacs/clang-format-region-or-buffer)))

(defun spacemacs/clang-format-on-save ()
  "Add before-save hook for clang-format."
  (add-hook 'before-save-hook 'spacemacs//clang-format-on-save nil t))

(defun spacemacs/company-more-than-prefix-guesser ()
  (spacemacs/c-c++-load-clang-args)
  (company-clang-guess-prefix))

;; Based on the Sarcasm/irony-mode compilation database code.
(defun spacemacs/company-find-clang-complete-file ()
  (when buffer-file-name
    (let ((dir (locate-dominating-file buffer-file-name ".clang_complete")))
      (when dir
        (concat (file-name-as-directory dir) ".clang_complete")))))

;; Based on the Sarcasm/irony-mode compilation database code.
(defun spacemacs/company-load-clang-complete-file (cc-file)
  "Load the flags from CC-FILE, one flag per line."
  (let ((invocation-dir (expand-file-name (file-name-directory cc-file)))
        (case-fold-search nil)
        (include-regex "\\(-I\\|-isystem\\|-iquote\\|-idirafter\\)\\s-*\\(\\S-+\\)")
        compile-flags)
    (with-temp-buffer
      (insert-file-contents cc-file)
      ;; Replace relative paths with absolute paths (by @trishume)
      ;; (goto-char (point-min))
      (while (re-search-forward include-regex nil t)
        (replace-match (format "%s%s" (match-string 1)
                               (expand-file-name (match-string 2)
                                                 invocation-dir))))
      ;; Turn lines into a list
      (setq compile-flags
            ;; remove whitespaces at the end of each line, if any
            (mapcar #'(lambda (line)
                        (if (string-match "[ \t]+$" line)
                            (replace-match "" t t line)
                          line))
                    (split-string (buffer-string) "\n" t))))
    compile-flags))

(defun spacemacs//c-c++-get-standard-include-paths (lang)
  "Returns the default system header include paths for LANG if gcc is on the
system and supports it, else returns a default set of include paths."
  (let* ((start "#include <...> search starts here:")
         (end "End of search list.")
         (gcc-tmplt "echo | gcc -x%s -E -v - 2>&1")
         (sed-tmplt " | sed -n '/%s/,/%s/{/%s/b;/%s/b;p}' | sed -e 's/^ *//g'")
         (template (concat gcc-tmplt sed-tmplt))
         (inc-dirs-cmd (format template lang start end start end))
         (inc-dirs (split-string (shell-command-to-string inc-dirs-cmd)
                                 "\n" t)))
    (if (and inc-dirs (every 'file-exists-p inc-dirs))
        inc-dirs
      '("/usr/include" "/usr/local/include"))))

(defun spacemacs//filter-and-substring (flags filter-prefix substr-index)
  "Returns all the strings in FLAGS starting with FILTER-PREFIX. The returned
strings are substringed from SUBSTR-INDEX inclusive to the end of the string."
  (mapcar (lambda (f) (substring f substr-index))
          (remove-if-not (lambda (f) (string-prefix-p filter-prefix f))
                         flags)))

(defun spacemacs/c-c++-load-clang-args ()
  "Sets the arguments for company-clang, the system paths for company-c-headers
and the arguments for flyckeck-clang based on a project-specific text file."
  (unless company-clang-arguments
    (let* ((cc-file (spacemacs/company-find-clang-complete-file))
           (flags (if cc-file
                      (spacemacs/company-load-clang-complete-file cc-file)
                    '()))
           (i-paths (spacemacs//filter-and-substring flags
                                                     "-I" 2))
           (iquote-paths (spacemacs//filter-and-substring flags
                                                          "-iquote" 7))
           (isystem-paths (spacemacs//filter-and-substring flags
                                                           "-isystem" 8))
           (idirafter-paths (spacemacs//filter-and-substring flags
                                                             "-idirafter" 10)))
      (setq-local company-clang-arguments flags)
      (setq-local flycheck-clang-args flags)
      (setq-local company-c-headers-path-user
                  (append '(".")
                          iquote-paths))
      (setq-local company-c-headers-path-system
                  (append i-paths
                          isystem-paths
                          (when (string-equal major-mode "c++-mode")
                            (spacemacs//c-c++-get-standard-include-paths "c++"))
                          (when (string-equal major-mode "c-mode")
                            (spacemacs//c-c++-get-standard-include-paths "c"))
                          idirafter-paths)))))


;; cpp-auto-include

(defalias 'spacemacs/c++-organize-includes 'cpp-auto-include)

(defun spacemacs//c++-organize-includes-on-save ()
  "Organize the includes on save when `c++-enable-organize-includes-on-save'
is non-nil."
  (when c++-enable-organize-includes-on-save
    (spacemacs/c++-organize-includes)))

(defun spacemacs/c++-organize-includes-on-save ()
  "Add before-save hook for c++-organize-includes."
  (add-hook 'before-save-hook
            #'spacemacs//c++-organize-includes-on-save nil t))


;; gtags

(defun spacemacs/c-c++-tags-find-symbol-at-point (&optional prefix)
  (interactive "P")
  (gtags-find-tag))

(defun spacemacs/c-c++-tags-find-references-at-point (&optional prefix)
  (interactive "P")
  (gtags-find-rtag))

(defun spacemacs/c-c++-tags-find-symbol ()
  (interactive)
  (call-interactively 'gtags-find-symbol))

(defun spacemacs/c-c++-tags-find-references ()
  (interactive)
  (call-interactively 'gtags-find-rtag))

(defun spacemacs/c-c++-tags-find-file ()
  (interactive)
  (call-interactively 'gtags-find-file))

(defun spacemacs/c-c++-tags-imenu ()
  (interactive)
  (call-interactively 'idomenu))


;; lsp

(defun spacemacs//c-c++-lsp-enabled ()
  "Return true if one or other of the lsp backends is enabled"
  (member c-c++-backend c-c++-lsp-backends))

;; -- BEGIN helper functions for common configuration of cquery and ccls backends
(defun spacemacs//c-c++-lsp-backend ()
  "Return a string representation of the LSP backend specified by the `c-c++-backend' configuration variable, without the `lsp-' prefix."
  (ecase c-c++-backend
    ('lsp-cquery "cquery")))

(defun spacemacs//c-c++-lsp-string (prefix suffix)
  (concat prefix (spacemacs//c-c++-lsp-backend) suffix))

(defun spacemacs//c-c++-lsp-symbol (prefix suffix)
  "Return a symbol for the LSP backend specified by the `c-c++-backend' configuration variable."
  (intern (spacemacs//c-c++-lsp-string prefix suffix)))

(defun spacemacs//c-c++-lsp-call-function (prefix suffix &rest args)
  (apply (spacemacs//c-c++-lsp-symbol prefix suffix) args))

(defun spacemacs//c-c++-lsp-funcall-interactively (prefix suffix &rest args)
  (funcall-interactively (spacemacs//c-c++-lsp-symbol prefix suffix) args))

(defun spacemacs//c-c++-lsp-funcall-interactively-no-args (prefix suffix)
  (funcall-interactively (spacemacs//c-c++-lsp-symbol prefix suffix)))

(defun spacemacs//c-c++-lsp-set-symbol (prefix suffix value)
  (set (spacemacs//c-c++-lsp-symbol prefix suffix) (symbol-value value)))

(defun spacemacs//c-c++-lsp-set-config (param prefix suffix)
  (when (symbol-value param) (spacemacs//c-c++-lsp-set-symbol prefix suffix param)))

(defun spacemacs//c-c++-lsp-apply-config (&rest parameters)
  (dolist (suffix parameters) (spacemacs//c-c++-lsp-set-config (intern (concat "c-c++-lsp-" suffix)) nil (concat "-" suffix))))
;; -- END helper functions for common configuration of cquery and ccls backends

(defun spacemacs//c-c++-lsp-config ()
  "Configure the LSP backend specified by the `c-c++-backend' configuration variable."
  (progn
    (spacemacs//c-c++-lsp-define-extensions)
    (spacemacs//c-c++-lsp-wrap-functions)
    (setq-default flycheck-disabled-checkers '(c/c++-clang c/c++-gcc))

    (spacemacs//c-c++-lsp-apply-config "executable" "initialization-options" "args" "project-whitelist" "project-blacklist" "sem-highlight-method")

    (if (eq c-c++-lsp-cache-dir nil)
        (progn
          (setq c-c++-lsp-cache-dir (file-truename(concat "~/.emacs.d/.cache/" (symbol-name c-c++-backend))))
          (message (concat "c-c++: No c-c++-lsp-cache-dir specified: defaulting to " c-c++-lsp-cache-dir))))

    (ecase c-c++-backend
      ('lsp-cquery (setq cquery-cache-dir c-c++-lsp-cache-dir)
                   (setq cquery-extra-args c-c++-lsp-args)
                   (setq cquery-extra-init-params
                         (if c-c++-lsp-initialization-options
                             (append c-c++-lsp-initialization-options '(:cacheFormat "msgpack"))
                           '(:cacheFormat "msgpack")))))

    (when c-c++-lsp-sem-highlight-rainbow
      (unless c-c++-lsp-sem-highlight-method
        (progn
          (setq c-c++-lsp-sem-highlight-method 'font-lock)
          (message "c-c++: No semantic highlight method specified. Defaulting to `font-lock'.")))
      (ecase c-c++-backend
        ('lsp-cquery (cquery-use-default-rainbow-sem-highlight))))

    (dolist (mode c-c++-modes)
      (spacemacs//c-c++-lsp-bind-keys-for-mode mode))

    (evil-set-initial-state '(spacemacs//c-c++-lsp-symbol nil "-tree-mode") 'emacs)
    ;;evil-record-macro keybinding clobbers q in cquery-tree-mode-map for some reason?
    (evil-make-overriding-map (symbol-value (spacemacs//c-c++-lsp-symbol nil "-tree-mode-map")))

    (if (configuration-layer/layer-used-p 'dap)
        (progn
          (require 'dap-gdb-lldb)
          (dolist (mode c-c++-modes)
            (spacemacs/dap-bind-keys-for-mode mode)))
      (message "`dap' layer is not installed, please add `dap' layer to your dotfile."))))

(defun spacemacs//c-c++-lsp-wrap-functions ()
  "Wrap navigation functions for the LSP backend specified by the `c-c++-backend' configuration variable."
  (defun c-c++/call-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-call-hierarchy"))
  (defun c-c++/call-hierarchy-inv () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-call-hierarchy" t))
  (defun c-c++/inheritance-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-inheritance-hierarchy"))
  (defun c-c++/inheritance-hierarchy-inv () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-inheritance-hierarchy" t))
  (defun c-c++/member-hierarchy () (interactive) (spacemacs//c-c++-lsp-funcall-interactively-no-args nil "-member-hierarchy"))
  (defun c-c++/preprocess-file () (interactive) (spacemacs//c-c++-lsp-funcall-interactively nil "-preprocess-file"))
  (defun c-c++/refresh-index () (interactive) ()
    (ecase c-c++-backend
      ('lsp-cquery (cquery-freshen-index)))))

(defun spacemacs//c-c++-lsp-bind-keys-for-mode (mode)
  "Bind LSP backend functions for the specified mode."
  (spacemacs/set-leader-keys-for-major-mode mode
    ;; backend
    "bf" #'c-c++/refresh-index
    "bp" #'c-c++/preprocess-file
    ;; goto
    "gf" 'find-file-at-point
    "gF" 'ffap-other-window
    ;; hierarchy
    "ghc" #'c-c++/call-hierarchy
    "ghC" #'c-c++/call-hierarchy-inv
    "ghi" #'c-c++/inheritance-hierarchy
    "ghI" #'c-c++/inheritance-hierarchy-inv
    ;; members
    "gmh" #'c-c++/member-hierarchy)

  ;; goto/peek
  (spacemacs/lsp-bind-extensions-for-mode mode "c-c++"
    "&" 'refs-address
    "R" 'refs-read
    "W" 'refs-write
    "c" 'callers
    "C" 'callees
    "v" 'vars
    "hb" 'base) ;;Replace this with lsp-goto-implementation in lsp-layer?

  )

(defun spacemacs//c-c++-lsp-define-extensions ()
  "Wrap some backend-specific extensions using the find functions provided by lsp-mode and lsp-ui"
  (spacemacs//c-c++-lsp-call-function "spacemacs//c-c++-lsp-define-" "-extensions")

  (spacemacs/lsp-define-extensions "c-c++" 'vars
    (spacemacs//c-c++-lsp-string "$" "/vars")))

(defun spacemacs//c-c++-lsp-define-cquery-extensions ()
  (spacemacs/lsp-define-extensions "c-c++" 'refs-address
    "textDocument/references"
    '(plist-put (lsp--text-document-position-params) :context '(:role 128)))
  (spacemacs/lsp-define-extensions "c-c++" 'refs-read
    "textDocument/references"
    '(plist-put (lsp--text-document-position-params) :context '(:role 8)))
  (spacemacs/lsp-define-extensions "c-c++" 'refs-write
    "textDocument/references"
    '(plist-put (lsp--text-document-position-params) :context '(:role 16)))
  (spacemacs/lsp-define-extensions "c-c++" 'callers "$cquery/callers")
  (spacemacs/lsp-define-extensions "c-c++" 'callees "$cquery/callers" '(:callee t))
  (spacemacs/lsp-define-extensions "c-c++" 'base "$cquery/base"))


;; cquery
(defun memacs/cquery-caller-hierarchy ()
  "caller hierarchy"
  (interactive)
  (cquery-call-hierarchy nil))

(defun memacs/cquery-callee-hierarchy ()
  "callee hierarchy"
  (interactive)
  (cquery-call-hierarchy t))

(defun memacs/cquery-base-inheritance-hierarchy ()
  "base inheritance"
  (interactive)
  (cquery-inheritance-hierarchy nil))

(defun memacs/cquery-derived-inheritance-hierarchy ()
  "derived inheritance"
  (interactive)
  (cquery-inheritance-hierarchy t))
