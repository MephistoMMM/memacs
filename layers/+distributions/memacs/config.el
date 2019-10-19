;;; memacs/config.el --- define configs for memacs layer

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: keywords

;;; Code:


;; Recomplie
(defconst memacs-recompile-list
  '(
    "core"
    "themes"
    "layers/+checkers"
    "layers/+completion"
    "layers/+distributions"
    "layers/+emacs"
    "layers/+misc"
    "layers/+os"
    "layers/+private"
    "layers/+readers"
    "layers/+source-control"
    "layers/+spacemacs"
    "layers/+tags"
    "layers/+vim"
    "layers/+tools"
    "layers/+lang"
    )
  "Files and dirs should be compiled.Format (path)")

;;; memacs/config.el ends here
