;;; memacs/keybindings.el --- define keybindings for memacs layer

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: memacs keybindings

;;; Code:


;;;; Recompile

(spacemacs/set-leader-keys
  "fec"  'memacs/recompile
  "fef"  'byte-recompile-file
  "feF"  'byte-recompile-directory
  )

;;; memacs/keybindings.el ends here
