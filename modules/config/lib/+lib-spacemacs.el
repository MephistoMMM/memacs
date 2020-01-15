;;; config/lib/+lib-spacemacs.el -*- lexical-binding: t; -*-

;; NOTE from spacemacs: `core/core-funcs.el'
(defun spacemacs//create-key-binding-form (props func)
  "Helper which returns a from to bind FUNC to a key according to PROPS.

Supported properties:

`:evil-leader STRING'
    One or several key sequence strings to be set with `spacemacs/set-leader-keys .

`:evil-leader-for-mode CONS CELL'
    One or several cons cells (MODE . KEY) where MODE is a major-mode symbol
    and KEY is a key sequence string to be set with
    `spacemacs/set-leader-keys-for-major-mode'.

`:global-key STRING'
    One or several key sequence strings to be set with `global-set-key'.

`:define-key CONS CELL'
    One or several cons cells (MAP . KEY) where MAP is a mode map and KEY is a
    key sequence string to be set with `define-key'. "
  (let ((evil-leader (spacemacs/mplist-get-values props :evil-leader))
        (evil-leader-for-mode (spacemacs/mplist-get-values props :evil-leader-for-mode))
        (global-key (spacemacs/mplist-get-values props :global-key))
        (def-key (spacemacs/mplist-get-values props :define-key)))
    (append
     (when evil-leader
       `((dolist (key ',evil-leader)
           (spacemacs/set-leader-keys key ',func))))
     (when evil-leader-for-mode
       `((dolist (val ',evil-leader-for-mode)
           (spacemacs/set-leader-keys-for-major-mode
             (car val) (cdr val) ',func))))
     (when global-key
       `((dolist (key ',global-key)
           (global-set-key (kbd key) ',func))))
     (when def-key
       `((dolist (val ',def-key)
           (define-key (eval (car val)) (kbd (cdr val)) ',func)))))))

(defun spacemacs/mplist-get-values (plist prop)
  "Get the values associated to PROP in PLIST, a modified plist.

A modified plist is one where keys are keywords and values are
all non-keywords elements that follow it.

If there are multiple properties with the same keyword, only the first property
and its values is returned.

Currently this function infloops when the list is circular."
  (let ((tail plist)
        result)
    (while (and (consp tail) (not (eq prop (car tail))))
      (pop tail))
    ;; pop the found keyword
    (pop tail)
    (while (and (consp tail) (not (keywordp (car tail))))
      (push (pop tail) result))
    (nreverse result)))
