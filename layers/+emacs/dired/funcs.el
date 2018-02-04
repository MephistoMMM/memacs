;;; funcs.el --- dired Layer funcs File for Spacemacs
;;
;; Copyright (c) 2014-2018 Sylvain Benner & Contributors
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;; 目錄排在檔案之前。
(defun memacs/dired-directory-sort ()
  "Dired sort hook to list directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max))))
  (and (featurep 'xemacs)
       (fboundp 'dired-insert-set-properties)
       (dired-insert-set-properties (point-min) (point-max)))
  (set-buffer-modified-p nil))

;; Dired Omit 加強:
;; 簡單來說，這個能夠紀錄下目前的「隱藏狀態」，所以當你按
;; , o 隱藏以.為開頭的檔案後，即使到了不同目錄下，以.開頭的檔案
;; 依舊是處於隱藏狀態，直到你重新按 , o 為止。
(defun memacs/dired-omit-and-remember ()
  "This function is a small enhancement for `dired-omit-mode', which will
        \"remember\" omit state across Dired buffers."
  (interactive)
  (setq memacs-dired-omit (not memacs-dired-omit))
  (memacs//dired-omit-auto-apply)
  (revert-buffer))
(defun memacs//dired-omit-auto-apply ()
  (setq dired-omit-mode memacs-dired-omit))

(defun memacs/dired-backward ()
  "Go back to the parent directory (..), and the cursor will be moved to where
          the previous directory."
  (interactive)
  (let* ((DIR (buffer-name)))
    (if (equal DIR "*Find*")
        (quit-window t)
      (progn (find-alternate-file "..")
             (re-search-forward DIR nil :no-error)
             (revert-buffer)))))
(defun memacs/dired-find-alternate-file ()
  (interactive)
  (if (file-regular-p (dired-get-filename))
      (dired-find-file)
    (dired-find-alternate-file)))
(put 'dired-find-alternate-file 'disabled nil)

;; 和 KDE 的 Dolphin 一樣的檔案名過濾器，按 C-i 使用。 (by letoh)
(defun memacs/dired-show-only (regexp)
  (interactive "sFiles to show (regexp): ")
  (dired-mark-files-regexp regexp)
  (dired-toggle-marks)
  (dired-do-kill-lines))

;; 使用 f 搜尋目前目錄
(defun memacs/dired-find-name-in-current-directory ()
  (interactive)
  (find-name-dired default-directory
                   (format "*%s*" (read-from-minibuffer "Pattern: ")))
  (set-buffer-multibyte t))

;; 手動開系統的外接硬碟掛載目錄很麻煩，乾脆弄個快速鍵，C-c m 直接開
;; /var/rum/media（如果你的系統掛載路徑不在這，請自行修改）
(defun memacs/dired-open-mounted-media-dir ()
  (interactive)
  (cond
   ((spacemacs/system-is-mac)
    (find-file "/Volumes"))
   ((spacemacs/system-is-linux)
    (find-file "/var/run/media/"))))

;; 按 , s 排序檔案，會先問你要根據什麼屬性排序，而且紀錄下排序狀態，不會
;; 跨 buffer 就不見了。
(defun memacs/dired-sort-size ()
  "Dired sort by size."
  (interactive) (dired-sort-other (concat dired-listing-switches "S")))
(defun memacs/dired-sort-extension ()
  "Dired sort by extension."
  (interactive) (dired-sort-other (concat dired-listing-switches "X")))
(defun memacs/dired-sort-ctime ()
  "Dired sort by create time."
  (interactive) (dired-sort-other (concat dired-listing-switches "ct")))
(defun memacs/dired-sort-utime ()
  "Dired sort by access time."
  (interactive) (dired-sort-other (concat dired-listing-switches "ut")))
(defun memacs/dired-sort-time ()
  "Dired sort by time."
  (interactive) (dired-sort-other (concat dired-listing-switches "t")))
(defun memacs/dired-sort-name ()
  "Dired sort by name."
  (interactive) (dired-sort-other (concat dired-listing-switches "")))

(defun memacs/dired-sort-and-remember ()
  (interactive)
  (setq memacs-dired-sort
        (intern
         (ivy-completing-read "Sort by: " '(name size extension ctime utime time))))
  (memacs//dired-sort-auto-apply))
(defun memacs//dired-sort-auto-apply ()
  (cond ((eq memacs-dired-sort 'name) (memacs/dired-sort-name))
        ((eq memacs-dired-sort 'size) (memacs/dired-sort-size))
        ((eq memacs-dired-sort 'extenstion) (memacs/dired-sort-extenstion))
        ((eq memacs-dired-sort 'ctime) (memacs/dired-sort-ctime))
        ((eq memacs-dired-sort 'utime) (memacs/dired-sort-utime))
        ((eq memacs-dired-sort 'time) (memacs/dired-sort-time))))
