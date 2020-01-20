;;; config/lib/+lib-memacs.el -*- lexical-binding: t; -*-

(defun memacs/file-contents (filename)
  "Return the contents of FILENAME.
Also see https://stackoverflow.com/questions/34432246/how-to-read-contents-of-the-file-programmatically-in-emacs"
  (with-temp-buffer
    (insert-file-contents-literally filename)
    (buffer-string)))

