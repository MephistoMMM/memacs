;;; spotlight-autoloads.el ---
;;
;; Copyright (c) 2015-2020 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3



;;; spotlight-autoloads.el ends here

;;;### (autoloads nil "spotlight" "spotlight.el" (24079 13636 901070
;;;;;;  429000))
;;; Generated autoloads from spotlight.el

(autoload 'spotlight "spotlight" "\
Search for a string ARG in the spotlight database.

Uses `ivy-read' to perform dynamic updates for each new character
entered.

You'll be given a list of files that match.  Selecting a file will
launch `swiper' for that file to search it for your query string.
INITIAL-INPUT can be given as the initial minibuffer input.

Customise the variable `spotlight-min-chars' to set the minimum
number of characters that must be entered before the first
spotlight search is performed.  Setting `spotlight-min-chars' to a
lower number will result in more matches and can lead to slow
performance.

Use \\<spotlight-map> \\[spotlight-launch-file-filter] to filter the list of matching files by filename.

If used with a prefix argument then it will prompt the user for a
base directory to search below, otherwise it will use
`spotlight-default-base-dir' as the base directory.

\(fn ARG &optional INITIAL-INPUT)" t nil)

(autoload 'spotlight-fast "spotlight" "\
Search for a string in the spotlight database.

You'll be given a list of files that match.  Narrow to the
filename you want by typing text to match the filename and then
selecting a file will launch `swiper' for that file to search for
your original query.

Optionally provide INITIAL-INPUT to specify the query string and
jump straight to the filename filter.

If used with a prefix argument then it will prompt the user for a
base directory to search below, otherwise it will use
`spotlight-default-base-dir' as the base directory.

\(fn ARG &optional INITIAL-INPUT)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "spotlight" '(#("spotlight-" 0 10 (fontified nil face font-lock-function-name-face)) #("ivy-mdfind-function" 0 19 (face font-lock-function-name-face fontified nil)))))

;;;***
