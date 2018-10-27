;;; config.el --- OSX Layer packages File for Spacemacs
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Boris Buliga <boris@d12frosted.io>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3


;;; Counsel Spotlight
;; Incrementally search the Mac spotlight database and open matching
;; files with a swiper search on the query text.
;; Based on http://oremacs.com/2015/07/27/counsel-recoll/

;; Function to be called by counsel-spotlight
;; The onlyin option limits results to my home directory
;; and directories below that
;; mdfind is the command-line interface to spotlight
(defun memacs/counsel-mdfind-function (string &rest _unused)
  "Issue mdfind for STRING."
  (or(counsel-more-chars)
     (let* ((memacs--counsel-search-cmd
             (format "mdfind -onlyin ~/ '%s'" string)))
       (spacemacs//counsel-async-command memacs--counsel-search-cmd)
       nil)))

;; Main function
(defun memacs/counsel-spotlight (&optional initial-input)
  "Search for a string in the mdfind database.
You'll be given a list of files that match.
Selecting a file will launch `swiper' for that file.
INITIAL-INPUT can be given as the initial minibuffer input."
  (interactive)
  (ivy-read (concat ivy-count-format
                    "Spotlight: ")
            'memacs/counsel-mdfind-function
            :initial-input initial-input
            :dynamic-collection t
            :caller 'memacs/counsel-spotlight
            :action (lambda (x)
                      (when (string-match "\\(\/.*\\)\\'" x)
                        (let ((file-name (match-string 1 x)))
                          (find-file file-name)
                          (unless (string-match "pdf$" x)
                            (swiper ivy-text)))))
            :keymap spacemacs--counsel-map
            :unwind (lambda ()
                      (counsel-delete-process)
                      (swiper--cleanup))))


;;; Autoescape

(defun memacs/autoescape-use-english-layout()
  "Change input source to english layout while emacs frame focused."
  (unless (evil-hybrid-state-p)
    (setq memacs-autoescape--origin-outside-layout-name (shell-command-to-string "textinputsource"))
    (unless (string= memacs-autoescape--origin-outside-layout-name
                     memacs-autoescape-english-layout-name)
      (start-process-shell-command "changeInputSource" nil
                                   (concat "textinputsource -s "
                                           memacs-autoescape-english-layout-name))
      ))
  )

(defun memacs/autoescape-recover-outside-layout()
  "Recover input source to origin layout while emacs frame unfocused."
  (unless (string=
           (shell-command-to-string "textinputsource")
           memacs-autoescape--origin-outside-layout-name)
    (call-process-shell-command (concat "textinputsource -s "
                                        memacs-autoescape--origin-outside-layout-name)))
  )



(defun memacs//switch-to-item2-run-command(CMD)
  "Open item2 run command CMD."
  (do-applescript
   (format "
          tell application \"iTerm\"
            activate
            try
              select first window
            on error
              create window with default profile
              select first window
            end try
            tell the first window
              tell current session to write text \"%s\"
            end tell
          end tell" CMD))
  )

(defun memacs/switch-to-item2-on-dir-of-current-buffer()
  "Open item2 then cd into the path of the directory of
current buffer."
  (interactive)
  (memacs//switch-to-item2-run-command
   (concat
    "cd "
    (replace-regexp-in-string "\\\\" "\\\\\\\\"
                              (shell-quote-argument
                               (or default-directory "~")))))
  )

/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/funcs.el
/Users/mephis/Library/Application Support/Sogou/InputMethod/SogouPY/sync.dat
/Users/mephis/Library/Application Support/Steam/logs/cef_log.txt
/Users/mephis/Library/Containers/com.reederapp.rkit2.mac/Data/Library/Application Support/Reeder/rkit/icons/fav/maiyang.me
/Users/mephis/Dropbox/org/notes/java/spring_aop.org
/Users/mephis/Dropbox/org/notes/validator.org
/Users/mephis/Dropbox/org/TODOs.org
/Users/mephis/Dropbox/org/notes/copy.org
/Users/mephis/Workspace/dockers/tmp/Dockerfile
/Users/mephis/Workspace/go/src/github.com/MephistoMMM/grafter/cmd/root.go
/Users/mephis/Library/Containers/com.atlassian.trello/Data/Library/Application Support/com.atlassian.trello/logs/main.log
/Users/mephis/Dropbox/Papers/阿里巴巴Java开发手册（详尽版）.pdf
/Users/mephis/Workspace/go/src/testbed/tmp.go
/Users/mephis/Library/Containers/com.xiami.client/Data/Library/Application Support/com.xiami.client/wvapp/zcache/zxiamimaccollect/0.2.1/9995ba7463d7a953b05462d4591bf63f
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/funcs.el
/Users/mephis/Library/Application Support/Sogou/InputMethod/SogouPY/sync.dat
/Users/mephis/Library/Application Support/Steam/logs/cef_log.txt
/Users/mephis/Library/Containers/com.reederapp.rkit2.mac/Data/Library/Application Support/Reeder/rkit/icons/fav/maiyang.me
/Users/mephis/Dropbox/org/notes/java/spring_aop.org
/Users/mephis/Dropbox/org/notes/validator.org
/Users/mephis/Dropbox/org/TODOs.org
/Users/mephis/Dropbox/org/notes/copy.org
/Users/mephis/Workspace/dockers/tmp/Dockerfile
/Users/mephis/Workspace/go/src/github.com/MephistoMMM/grafter/cmd/root.go
/Users/mephis/Library/Containers/com.atlassian.trello/Data/Library/Application Support/com.atlassian.trello/logs/main.log
/Users/mephis/Dropbox/Papers/阿里巴巴Java开发手册（详尽版）.pdf
/Users/mephis/Workspace/go/src/testbed/tmp.go
/Users/mephis/Library/Containers/com.xiami.client/Data/Library/Application Support/com.xiami.client/wvapp/zcache/zxiamimaccollect/0.2.1/9995ba7463d7a953b05462d4591bf63f
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/funcs.el
/Users/mephis/Library/Application Support/Sogou/InputMethod/SogouPY/sync.dat
/Users/mephis/Library/Application Support/Steam/logs/cef_log.txt
/Users/mephis/Library/Containers/com.reederapp.rkit2.mac/Data/Library/Application Support/Reeder/rkit/icons/fav/maiyang.me
/Users/mephis/Dropbox/org/notes/java/spring_aop.org
/Users/mephis/Dropbox/org/notes/validator.org
/Users/mephis/Dropbox/org/TODOs.org
/Users/mephis/Dropbox/org/notes/copy.org
/Users/mephis/Workspace/dockers/tmp/Dockerfile
/Users/mephis/Workspace/go/src/github.com/MephistoMMM/grafter/cmd/root.go
/Users/mephis/Library/Containers/com.atlassian.trello/Data/Library/Application Support/com.atlassian.trello/logs/main.log
/Users/mephis/Dropbox/Papers/阿里巴巴Java开发手册（详尽版）.pdf
/Users/mephis/Workspace/go/src/testbed/tmp.go
/Users/mephis/Library/Containers/com.xiami.client/Data/Library/Application Support/com.xiami.client/wvapp/zcache/zxiamimaccollect/0.2.1/9995ba7463d7a953b05462d4591bf63f

/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
/Users/mephis/Workspace/emacs/memacs/layers/+tools/debug/img/debug.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/tern/img/tern.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/node/img/node.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/cmake/img/cmake.png
/Users/mephis/Workspace/emacs/memacs/layers/+tools/docker/img/docker.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/osx.png
/Users/mephis/Workspace/emacs/memacs/layers/+os/osx/img/apple.png
/Users/mephis/Workspace/emacs/memacs/layers/+music/spotify/img/spotify.png
/Users/mephis/Workspace/emacs/memacs/layers/+checkers/syntax-checking/img/flycheck.png
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/dired/packages.el
/Users/mephis/Workspace/emacs/memacs/layers/+emacs/org/img/org.png
/Users/mephis/Workspace/emacs/memacs/layers/+readers/pdf/img/pdf.png
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/org-effectiveness.el
/Users/mephis/Workspace/emacs/master/elpa/develop/org-plus-contrib-20180917/ob-mathomatic.el
