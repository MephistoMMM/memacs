;;; funcs.el --- provide functions for pretty-code Layer
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

(defun pretty-code//unicode-setup-fonts-h (&optional frame)
  "Initialize  `unicode-fonts', if in a Gui session"
  (when (and frame (display-graphic-p frame))
    (with-selected-frame
        (require 'unicode-fonts)
      ;; NOTE will impact startup time on first run
      (unicode-fonts-setup))))

;;; funcs.el ends here
