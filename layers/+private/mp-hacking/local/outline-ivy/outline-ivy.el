;;; outline-ivy.el --- package provides methods for jumping to outlines

;; Origin Author: ekaschalk (http://modernemacs.com)

;; Author: Mephis Pheies <mephistommm@gmail.com>
;; Keywords: outshine ivy outline-ivy

;;; License:

;; MIT

;;; Code:

(require 'ivy)
(require 'outshine)

;;;; Config

(defvar oi-height 20
  "Number of outlines to display, overrides ivy-height.")

(defface oi-match-face
  '((t :inherit match))
  "Match face for ivy outline prompt.")

(defface oi-face-1
  '((t :foreground "#C3A29E" :height 1.25 :underline t :weight ultra-bold))
  "Ivy outline face for level 1")

(defface oi-face-2
  '((t :foreground "#8D6B94" :height 1.15 :weight semi-bold))
  "Ivy outline face for level 2")

(defface oi-face-3
  '((t :foreground "#8C5f66"))
  "Ivy outline face for level 3")

;;;; Utils

;;;###autoload
(defun oi-rgx ()
  "Regex to match outlines with first group as its text."
  (cadar outshine-imenu-preliminary-generic-expression))

;;;###autoload
(defun oi-format-name (STR LEVEL)
  "Format STR at LEVEL for ivy."
  (pcase LEVEL
    (2 (format " %s" STR))
    (3 (format "  %s" STR))
    (_ STR)))

;;;###autoload
(defun oi-format-name-pretty (STR PARENTS LEVEL)
  "Prepend invisible PARENTS to propertized STR at LEVEL."
  (concat (propertize
           (concat (when LEVEL (number-to-string LEVEL))
                   (apply 'concat PARENTS))
           'invisible t)
          (propertize (oi-format-name STR LEVEL)
                      'face (pcase LEVEL
                              (1 'oi-face-1)
                              (2 'oi-face-2)
                              (3 'oi-face-3)))))

;;;###autoload
(defun oi--collect-outline ()
  "Collect outline-ivy formatted outline string and marker for line at point."
  (save-excursion
    (beginning-of-line)
    (-let* ((level (outshine-calc-outline-level))
            (parents (when level
                       (--map (plist-get oi--parents-plist it)
                             (number-sequence 1 (- level 1)))))
            (str (match-string-no-properties 1))
            (name (oi-format-name-pretty str parents level)))
      (when level
        (setq oi--parents-plist (plist-put oi--parents-plist level str)))
      (->> (point-marker)
         (cons name)
         (when level)))))

;;;###autoload
(defun oi-collect-outlines ()
  "Collect fontified outline strings and their markers for ivy-read."
  (setq oi--parents-plist nil)
  (save-excursion
    (goto-char (point-min))
    (-snoc (--unfold
            (when (search-forward-regexp (oi-rgx) nil t)
              (cons it (oi--collect-outline)))
            nil)
           (oi--collect-outline))))

;;;; Outline Jump

;;;###autoload
(defun oi--preselect ()
  "Get parent outline at point for ivy :preselect."
  (save-excursion
    (unless (outline-on-heading-p t)
      (outline-previous-heading))
    (search-forward-regexp (oi-rgx) nil t)
    (beginning-of-line)
    (-> (match-string-no-properties 1)
       (oi-format-name (outshine-calc-outline-level)))))

;;;###autoload
(defun oi--remap-ivy-match-face ()
  "Overwrite ivy-current-match face in outline-ivy prompt."
  (set (make-local-variable 'face-remapping-alist)
       '((ivy-current-match oi-match-face))))

;;;###autoload
(defun oi-jump ()
  "Prompt fontified, hierarchal outlines for jump."
  (interactive)
  (let ((ivy-height oi-height))
    (ivy-read "Outline " (oi-collect-outlines)
              :preselect (oi--preselect)
              :update-fn 'oi--remap-ivy-match-face
              :action (-lambda ((_ . marker))
                        (with-ivy-window
                          (-> marker marker-position goto-char)
                          (recenter 2))))))
(provide 'outline-ivy)

;;; outline-ivy.el ends here
