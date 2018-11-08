;;; memacs/keybindings.el --- define keybindings for memacs layer

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: memacs keybindings

;;; Code:


;;;; Spaceline toggles
(dolist (spec '((minor-modes "tmm")
                (major-mode "tmM")
                (version-control "tmv")
                (new-version "tmV")
                (point-position "tmp")
                (org-clock "tmc")))
  (let* ((segment (car spec))
         (status-var (intern (format "spaceline-%S-p" segment))))
    (eval `(spacemacs|add-toggle ,(intern (format "mode-line-%S" segment))
             :status ,status-var
             :on (setq ,status-var t)
             :off (setq ,status-var nil)
             :documentation ,(format "Show %s in the mode-line."
                                     (replace-regexp-in-string
                                      "-" " " (format "%S" segment)))
             :evil-leader ,(cadr spec)))))


;;;; Swiper

(memacs/define-evil-keybinding
 (list evil-normal-state-map evil-evilified-state-map)
 "/" 'swiper)


;;;; Recompile

(spacemacs/set-leader-keys
  "fec"  'memacs/recompile
  "fef"  'byte-recompile-file
  "feF"  'byte-recompile-directory
  )

;;; memacs/keybindings.el ends here
