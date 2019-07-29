;;; core-dotspacemacs.el --- Spacemacs Core File
;;
;; Copyright (c) 2012-2018 Sylvain Benner & Contributors
;;
;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3
(defconst dotspacemacs-test-results-buffer "*dotfile-test-results*"
  "Name of the buffer to display dotfile test results.")

(let* ((default-init (expand-file-name "memacs" spacemacs-start-directory)))
  (defvar dotspacemacs-filepath default-init)
  "Filepath to the installed dotfile. ~/.emacs.d/memacs")

(defvar dotspacemacs--user-config-elapsed-time 0
  "Time spent in `dotspacemacs/user-config' function.
Useful for users in order to given them a hint of potential bottleneck in
their configuration.")

(defvar dotspacemacs-distribution 'spacemacs-base
  "Base distribution to use. This is a layer contained in the directory
`+distributions'. For now available distributions are `spacemacs-base'
or `spacemacs'.")

(defvar dotspacemacs-enable-emacs-pdumper nil
  "If non-nil then enable support for the portable dumper. You'll need
to compile Emacs 27 from source following the instructions in file
EXPERIMENTAL.org at to root of the git repository.")

(defvar dotspacemacs-emacs-pdumper-executable-file "emacs"
  "File path pointing to emacs 27 or later executable.")

(defvar dotspacemacs-emacs-dumper-dump-file "spacemacs.pdmp"
  "Name of the Spacemacs dump file. This is the file will be created by the
portable dumper in the cache directory under dumps sub-directory.
To load it when starting Emacs add the parameter `--dump-file'
when invoking Emacs 27.1 executable on the command line, for instance:
./emacs --dump-file=/Users/sylvain/.emacs.d/.cache/dumps/spacemacs.pdmp")

(defvar dotspacemacs-gc-cons '(100000000 0.1)
  "Set `gc-cons-threshold' and `gc-cons-percentage' when startup finishes.
This is an advanced option and should not be changed unless you suspect
performance issues due to garbage collection operations.")

(defvar dotspacemacs-elpa-https t
  "If non nil ELPA repositories are contacted via HTTPS whenever it's
possible. Set it to nil if you have no way to use HTTPS in your
environment, otherwise it is strongly recommended to let it set to t.")

(defvar dotspacemacs-elpa-timeout 5
  "Maximum allowed time in seconds to contact an ELPA repository.")

(defvar dotspacemacs-use-spacelpa nil
  "If non-nil then Spacelpa repository is the primary source to install
a locked version of packages. If nil then Spacemacs will install the latest
version of packages from MELPA.")

(defvar dotspacemacs-verify-spacelpa-archives nil
  "If non-nil then verify the signature for downloaded Spacelpa archives.")

(defvar dotspacemacs-elpa-subdirectory nil
  "If non-nil, a form that evaluates to a package directory. For
example, to use different package directories for different Emacs
versions, set this to `emacs-version'.")

(defvar dotspacemacs-configuration-layer-path '()
  "List of additional paths where to look for configuration layers.
Paths must have a trailing slash (ie. `~/.mycontribs/')")

(defvar dotspacemacs-install-packages 'used-only
  "Defines the behaviour of Spacemacs when installing packages.
Possible values are `used-only', `used-but-keep-unused' and `all'. `used-only'
installs only explicitly used packages and deletes any unused packages as well
as their unused dependencies. `used-but-keep-unused' installs only the used
packages but won't delete unused ones. `all' installs *all*
packages supported by Spacemacs and never uninstalls them.")

(defvar dotspacemacs-additional-packages '()
  "List of additional packages that will be installed wihout being
wrapped in a layer. If you need some configuration for these
packages then consider to create a layer, you can also put the
configuration in `dotspacemacs/user-config'.")

(defvar dotspacemacs--additional-theme-packages '()
  "Same as `dotspacemacs-additional-packages' but reserved for themes declared
in `dotspacemacs-themes'.")

(defvar dotspacemacs-scratch-mode 'text-mode
  "Default major mode of the scratch buffer.")

(defvar dotspacemacs-initial-scratch-message 'nil
  "Initial message in the scratch buffer.")

(defvar dotspacemacs-configuration-layers '(emacs-lisp)
  "List of configuration layers to load.")

(defvar dotspacemacs--configuration-layers-saved nil
  "Saved value of `dotspacemacs-configuration-layers' after sync.")

(defvar dotspacemacs-themes '(dracula
                              spacemacs-light)
  "List of themes, the first of the list is loaded when spacemacs starts.
Press `SPC T n' to cycle to the next theme in the list (works great
with 2 themes variants, one dark and one light")

(defvar dotspacemacs-mode-line-theme '(memacs :separator wave :separator-scale 1.5)
  "Set the theme for the Spaceline. Supported themes are `spacemacs',
`all-the-icons', `custom', `doom',`vim-powerline' and `vanilla'. The first three
are spaceline themes. `doom' is the doom-emacs mode-line. `vanilla' is default
Emacs mode-line. `custom' is a user defined themes, refer to the
DOCUMENTATION.org for more info on how to create your own spaceline theme. Value
can be a symbol or a list with additional properties like '(all-the-icons
:separator-scale 1.5).")

(defvar dotspacemacs-frame-title-format "%I@%S"
  "Default format string for a frame title bar, using the
  original format spec, and additional customizations.")

(defvar dotspacemacs-icon-title-format nil
  "Default format string for a icon title bar, using the
  original format spec, and additional customizations.")

(defvar dotspacemacs-colorize-cursor-according-to-state t
  "If non nil the cursor color matches the state color in GUI Emacs.")

(defvar dotspacemacs-leader-key "SPC"
  "The leader key.")

(defvar dotspacemacs-emacs-leader-key "M-m"
  "The leader key accessible in `emacs state' and `insert state'")

(defvar dotspacemacs-major-mode-leader-key ","
  "Major mode leader key is a shortcut key which is the equivalent of
pressing `<leader> m`. Set it to `nil` to disable it.")

(defvar dotspacemacs-major-mode-emacs-leader-key "C-M-m"
  "Major mode leader key accessible in `emacs state' and `insert state'")

(defvar dotspacemacs-ex-command-key ":"
  "The key used for Vim Ex commands.")

(defvar dotspacemacs-command-key "SPC"
  "The key used for Emacs commands (M-x) (after pressing on the leader key).")
(defvaralias 'dotspacemacs-emacs-command-key 'dotspacemacs-command-key
  "New official name for `dotspacemacs-command-key'")

(defvar dotspacemacs-default-font '("Source Code Pro"
                                    :size 10.0
                                    :weight normal
                                    :width normal)
  "Default font, or prioritized list of fonts. This setting has no effect when
running Emacs in terminal.")

(defvar dotspacemacs-folding-method 'evil
  "Code folding method. Possible values are `evil' and `origami'.")

(defvar dotspacemacs-default-layout-name "Default"
  " Name of the default layout.")

(defvar dotspacemacs-display-default-layout t
  "If non nil the default layout name is displayed in the mode-line.")

(defvar dotspacemacs-auto-resume-layouts nil
  "If non nil then the last auto saved layouts are resume automatically upon
start.")

(defvar dotspacemacs-auto-generate-layout-names nil
  "If non-nil, auto-generate layout name when creating new layouts.
Only has effect when using the \"jump to layout by number\" commands.")

(defvar dotspacemacs-max-rollback-slots 5
  "Maximum number of rollback slots to keep in the cache.")

(defvar dotspacemacs-large-file-size 2
  "Size (in MB) above which spacemacs will prompt to open the large file
literally to avoid performance issues. Opening a file literally means that
no major mode or minor modes are active.")

(defvar dotspacemacs-auto-save-file-location 'cache
  "Location where to auto-save files. Possible values are `original' to
auto-save the file in-place, `cache' to auto-save the file to another
file stored in the cache directory and `nil' to disable auto-saving.")

(defvar dotspacemacs-which-key-delay 0.4
  "Delay in seconds starting from the last keystroke after which
the which-key buffer will be shown if you have not completed a
key sequence. Setting this variable is equivalent to setting
`which-key-idle-delay'.")

(defvar dotspacemacs-which-key-position 'bottom
  "Location of the which-key popup buffer. Possible choices are bottom,
right, and right-then-bottom. The last one will display on the
right if possible and fallback to bottom if not.")

(defvar dotspacemacs-loading-progress-bar t
  "If non nil a progress bar is displayed when spacemacs is loading. This
may increase the boot time on some systems and emacs builds, set it to nil
to boost the loading time.")

(defvar dotspacemacs-fullscreen-at-startup t
  "If non nil the frame is fullscreen when Emacs starts up (Emacs 24.4+ only).")

(defvar dotspacemacs-maximized-at-startup nil
  "If non nil the frame is maximized when Emacs starts up (Emacs 24.4+ only).
Takes effect only if `dotspacemacs-fullscreen-at-startup' is nil.")

(defvar dotspacemacs-undecorated-at-startup nil
  "If non nil the frame is undecorated when Emacs starts up.")

(defvar dotspacemacs-active-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's active or selected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotspacemacs-inactive-transparency 90
  "A value from the range (0..100), in increasing opacity, which describes the
transparency level of a frame when it's inactive or deselected. Transparency
can be toggled through `toggle-transparency'.")

(defvar dotspacemacs-show-transient-state-title t
  "If non nil show the titles of transient states.")

(defvar dotspacemacs-show-transient-state-color-guide t
  "If non nil show the color guide hint for transient state keys.")

(defvar dotspacemacs-mode-line-unicode-symbols t
  "If non nil unicode symbols are displayed in the mode-line (eg. for lighters).
If you use Emacs as a daemon and wants unicode characters only in GUI set
the value to quoted `display-graphic-p'. (default t)")

(defvar dotspacemacs-smooth-scrolling t
  "If non nil smooth scrolling (native-scrolling) is enabled.
Smooth scrolling overrides the default behavior of Emacs which
recenters point when it reaches the top or bottom of the
screen.")

(defvar dotspacemacs-line-numbers nil
  "Control line numbers activation.
If set to `t' or `relative' line numbers are turned on in all `prog-mode' and
`text-mode' derivatives. If set to `relative', line numbers are relative.
This variable can also be set to a property list for finer control:
'(:relative nil
  :disabled-for-modes dired-mode
                      doc-view-mode
                      markdown-mode
                      org-mode
                      pdf-view-mode
                      text-mode
  :size-limit-kb 1000)
The property `:enabled-for-modes' takes priority over `:disabled-for-modes' and
restricts line-number to the specified list of major-mode.")

(defvar dotspacemacs-enable-server nil
  "If non-nil, start an Emacs server if one is not already running.")

(defvar dotspacemacs-persistent-server nil
  "If non nil advises quit functions to keep server open when quitting.")

(defvar dotspacemacs-server-socket-dir nil
  "Set the emacs server socket location.
If nil, uses whatever the Emacs default is,
otherwise a directory path like \"~/.emacs.d/server\".
Has no effect if `dotspacemacs-enable-server' is nil.")

(defvar dotspacemacs-smart-closing-parenthesis nil
  "If non-nil pressing the closing parenthesis `)' key in insert mode passes
over any automatically added closing parenthesis, bracket, quote, etc…
This can be temporary disabled by pressing `C-q' before `)'.")

(defvar dotspacemacs-highlight-delimiters 'current
  "Select a scope to highlight delimiters. Possible values are `any',
`current', `all' or `nil'. Default is `all' (highlight any scope and
emphasize the current one.")

(defvar dotspacemacs-whitespace-cleanup 'changed
  "delete whitespace while saving buffer. possible values are `all'
to aggressively delete empty lines and long sequences of whitespace, `trailing'
to delete only the whitespace at end of lines, `changed' to delete only
whitespace for changed lines or `nil' to disable cleanup.")

(defvar dotspacemacs-search-tools '("rg" "ag" "grep")
  "List of search tool executable names. Spacemacs uses the first installed
tool of the list. Supported tools are `rg', `ag' and `grep'.")

(defvar dotspacemacs-default-package-repository 'melpa-stable
  "The default package repository used if no explicit repository has been
specified with an installed package.
NOT USED FOR NOW :-)")

(defvar dotspacemacs-startup-lists '((recents  . 5)
                                    (projects . 7))
  "Association list of items to show in the startup buffer of the form
`(list-type . list-size)`. If nil it is disabled.
Possible values for list-type are:
`recents' `bookmarks' `projects' `agenda' `todos'.
List sizes may be nil, in which case
`spacemacs--buffer-startup-lists-length' takes effect.
")

(defvar dotspacemacs-startup-buffer-responsive t
  "True if the home buffer should respond to resize events.")

(defvar dotspacemacs-excluded-packages '()
  "A list of packages that will not be installed and loaded.")

(defvar dotspacemacs-frozen-packages '()
  "A list of packages that cannot be updated.")

(defvar dotspacemacs-pretty-docs nil
  "Run `spacemacs/prettify-org-buffer' when
visiting README.org files of Spacemacs.")

(defvar dotmemacs-username "username"
  "Username of memacs user.")

(defvar dotmemacs-email "user@email.com"
  "Email of memacs user.")

(defvar dotmemacs-github "https://gibhub.com/user"
  "Github of memacs user.")

(defvar dotspacemacs-new-empty-buffer-major-mode nil
  "Set the major mode for a new empty buffer.")

;; only for backward compatibility
(defalias 'dotspacemacs-mode 'emacs-lisp-mode)

(defmacro dotspacemacs|call-func (func &optional msg)
  "Call the function from the dotfile only if it is bound.
If MSG is not nil then display a message in `*Messages*'. Errors
are caught and signaled to user in spacemacs buffer."
  `(progn
     (when ,msg (spacemacs-buffer/message ,msg))
     (when (fboundp ',func)
       (condition-case-unless-debug err
           (,func)
         (error
          (configuration-layer//increment-error-count)
          (spacemacs-buffer/append (format "Error in %s: %s\n"
                                           ',(symbol-name func)
                                           (error-message-string err))
                                   t))))))

(defun dotspacemacs/call-user-env ()
  "Call the function `dotspacemacs/user-env'."
  (interactive)
  (dotspacemacs|call-func dotspacemacs/user-env "Calling dotfile user env..."))

(defun dotspacemacs/go-to-function (func)
  "Open the dotfile and goes to FUNC function."
  (interactive)
  (find-function func))

(defun dotspacemacs/go-to-user-env ()
  "Go to the `dotspacemacs/user-env' function."
  (interactive)
  (dotspacemacs/go-to-function 'dotspacemacs/user-env))

(defun dotspacemacs//check-layers-changed ()
  "Check if the value of `dotspacemacs-configuration-layers'
changed, and issue a warning if it did."
  (unless (eq dotspacemacs-configuration-layers
              dotspacemacs--configuration-layers-saved)
    (spacemacs-buffer/warning
     "`dotspacemacs-configuration-layers' was changed outside of `dotspacemacs/layers'.")))
(add-hook 'spacemacs-post-user-config-hook
          'dotspacemacs//check-layers-changed)

(defun dotspacemacs/add-layer (layer-name)
  "Add LAYER_NAME to dotfile and reload the it.
Returns non nil if the layer has been effectively inserted."
  (unless (configuration-layer/layer-used-p layer-name)
    (with-current-buffer (find-file-noselect (dotspacemacs/location))
      (beginning-of-buffer)
      (let ((insert-point (re-search-forward
                           "[^`]dotspacemacs-configuration-layers\\s-*\n?[^(]*\\((\\)")))
        (insert (format "%S" layer-name))
        (unless (equal (point) (point-at-eol))
          (insert "\n"))
        (indent-region insert-point (min (point-max)
                                         (+ insert-point 2
                                            (length (symbol-name layer-name)))))
        (save-buffer)))
    (load-file (dotspacemacs/location))
    t))

(defun dotspacemacs//profile-user-config (f &rest args)
  "Compute time taken by the `dotspacemacs/user-config' function.
Set the variable"
  (let ((stime (current-time)))
    (apply f args)
    (setq dotspacemacs--user-config-elapsed-time
          (float-time (time-subtract (current-time) stime)))))

(defun dotspacemacs/sync-configuration-layers (&optional arg)
  "Synchronize declared layers in dotfile with spacemacs.

Called with `C-u' skips `dotspacemacs/user-config'.
Called with `C-u C-u' skips `dotspacemacs/user-config' _and_ preliminary tests."
  (interactive "P")
  (when (file-exists-p dotspacemacs-filepath)
    (with-current-buffer (find-file-noselect dotspacemacs-filepath)
      (let ((dotspacemacs-loading-progress-bar nil))
        (save-buffer)
        (let ((tests-ok (or (equal arg '(16)) (dotspacemacs/test-dotfile t))))
          (if tests-ok
              (progn
                (load-file buffer-file-name)
                (dotspacemacs|call-func dotspacemacs/init
                                        "Calling dotfile init...")
                (dotspacemacs|call-func dotspacemacs/user-init
                                        "Calling dotfile user init...")
                (dotspacemacs/call-user-env)
                ;; try to force a redump when reloading the configuration
                (let ((spacemacs-force-dump t))
                  (configuration-layer/load))
                (if (member arg '((4) (16)))
                    (message (concat "Done (`dotspacemacs/user-config' "
                                     "function has been skipped)."))
                  (dotspacemacs|call-func dotspacemacs/user-config
                                          "Calling dotfile user config...")
                  (run-hooks 'spacemacs-post-user-config-hook)
                  (message "Done.")))
            (switch-to-buffer-other-window dotspacemacs-test-results-buffer)
            (spacemacs-buffer/warning "Some tests failed, check `%s' buffer"
                                      dotspacemacs-test-results-buffer))))))
  (when (configuration-layer/package-used-p 'spaceline)
    (spacemacs//restore-buffers-powerline)))

(defun dotspacemacs/get-variable-string-list ()
  "Return a list of all the dotspacemacs variables as strings."
  (all-completions "" obarray
                   (lambda (x)
                     (and (boundp x)
                          (not (keywordp x))
                          ;; avoid private variables to show up
                          (not (string-match-p "--" (symbol-name x)))
                          (string-prefix-p "dotspacemacs" (symbol-name x))))))

(defun dotspacemacs/get-variable-list ()
  "Return a list of all dotspacemacs variable symbols."
  (mapcar 'intern (dotspacemacs/get-variable-string-list)))

(defmacro dotspacemacs|symbol-value (symbol)
  "Return the value of SYMBOL corresponding to a dotspacemacs variable.
If SYMBOL value is `display-graphic-p' then return the result of
 `(display-graphic-p)', otherwise return the value of the symbol."
  `(if (eq 'display-graphic-p ,symbol) (display-graphic-p) ,symbol))

(defun dotspacemacs/location ()
  "Return the absolute path to the spacemacs dotfile."
  dotspacemacs-filepath)

(defun dotspacemacs/load-file ()
  "Load ~/.spacemacs if it exists."
  (let ((dotspacemacs (dotspacemacs/location)))
    (if (file-exists-p dotspacemacs)
        (unless (with-demoted-errors "Error loading .spacemacs: %S"
                  (load dotspacemacs)))))
  (advice-add 'dotspacemacs/user-config :around 'dotspacemacs//profile-user-config))

(defun spacemacs/title-prepare (title-format)
  "A string is printed verbatim except for %-constructs.
  %a -- prints the `abbreviated-file-name', or `buffer-name'
  %t -- prints `projectile-project-name'
  %I -- prints `invocation-name'
  %S -- prints `system-name'
  %U -- prints contents of $USER
  %b -- prints buffer name
  %f -- prints visited file name
  %F -- prints frame name
  %s -- prints process status
  %p -- prints percent of buffer above top of window, or Top, Bot or All
  %P -- prints percent of buffer above bottom of window, perhaps plus Top, or
  print Bottom or All
  %m -- prints mode name
  %n -- prints Narrow if appropriate
  %z -- prints mnemonics of buffer, terminal, and keyboard coding systems
  %Z -- like %z, but including the end-of-line format"
  ;; save-match-data to work around Emacs bug, see
  ;; https://github.com/syl20bnr/spacemacs/issues/9700
  (save-match-data
    ;; disable buffer-list-update-hook to work around recursive invocations caused
    ;; by the temp-buffer used by `format-spec' below, see
    ;; https://github.com/syl20bnr/spacemacs/issues/12387
    (let* ((buffer-list-update-hook nil)
           (project-name (when (string-match-p "%t" title-format)
                           (if (boundp 'spacemacs--buffer-project-name)
                               spacemacs--buffer-project-name
                             (set (make-local-variable 'spacemacs--buffer-project-name)
                                  (if (fboundp 'projectile-project-name)
                                      (projectile-project-name)
                                    "-")))))
           (abbreviated-file-name (when (string-match-p "%a" title-format)
                                    (if (boundp 'spacemacs--buffer-abbreviated-filename)
                                        spacemacs--buffer-abbreviated-filename
                                      (set (make-local-variable 'spacemacs--buffer-abbreviated-filename)
                                           (abbreviate-file-name (or (buffer-file-name)
                                                                     (buffer-name)))))))
           (fs (format-spec-make
                ?a abbreviated-file-name
                ?t project-name
                ?S system-name
                ?I invocation-name
                ?U (or (getenv "USER") "")
                ?b "%b"
                ?f "%f"
                ?F "%F"
                ?* "%*"
                ?+ "%+"
                ?s "%s"
                ?l "%l"
                ?c "%c"
                ?p "%p"
                ?P "%P"
                ?m "%m"
                ?n "%n"
                ?z "%z"
                ?Z "%Z"
                ?\[ "%["
                ?\] "%]"
                ?% "%%"
                ?- "%-"
                )))
      (format-spec title-format fs))))

(defun spacemacs/get-mode-line-theme-name ()
  "Return the mode-line theme name."
  (if (listp dotspacemacs-mode-line-theme)
      (car dotspacemacs-mode-line-theme)
    dotspacemacs-mode-line-theme))

(defun spacemacs/mode-line-separator ()
  "Return the separator type for the mode-line.
Return nil if no separator is defined."
  (when (listp dotspacemacs-mode-line-theme)
    (plist-get (cdr dotspacemacs-mode-line-theme) :separator)))

(defun spacemacs/mode-line-separator-scale ()
  "Return the separator scale for the mode-line.
Return nil if no scale is defined."
  (when (listp dotspacemacs-mode-line-theme)
    (plist-get (cdr dotspacemacs-mode-line-theme) :separator-scale)))

(defun dotspacemacs//test-dotspacemacs/layers ()
  "Tests for `dotspacemacs/layers'"
  (insert
   (format (concat "\n* Testing settings in dotspacemacs/layers "
                   "[[file:%s::dotspacemacs/layers][Show in File]]\n")
           dotspacemacs-filepath))
  ;; protect global values of these variables
  (let (dotspacemacs-configuration-layer-path dotspacemacs-configuration-layers
        dotspacemacs-additional-packages dotspacemacs-excluded-packages
        dotspacemacs-install-packages
        (passed-tests 0) (total-tests 0))
    (load dotspacemacs-filepath)
    (dotspacemacs/layers)
    (spacemacs//test-list
     'stringp 'dotspacemacs-configuration-layer-path
     "is a string" "path")
    (spacemacs//test-list
     'file-directory-p 'dotspacemacs-configuration-layer-path
     "exists in filesystem" "path")
    (setq dotspacemacs-configuration-layers
          (mapcar (lambda (l) (if (listp l) (car l) l))
                  dotspacemacs-configuration-layers))
    (spacemacs//test-list
     'configuration-layer/get-layer-path
     'dotspacemacs-configuration-layers  "can be found" "layer")
    (insert (format
             (concat "** RESULTS: "
                     "[[file:%s::dotspacemacs/layers][dotspacemacs/layers]] "
                     "passed %s out of %s tests\n")
             dotspacemacs-filepath passed-tests total-tests))
    (equal passed-tests total-tests)))

(defmacro dotspacemacs||let-init-test (&rest body)
  "Macro to protect dotspacemacs variables"
  `(let ((fpath dotspacemacs-filepath)
         ,@(dotspacemacs/get-variable-list)
         (passed-tests 0) (total-tests 0))
     (setq dotspacemacs-filepath fpath)
     (load dotspacemacs-filepath)
     ,@body))

(defun dotspacemacs//test-dotspacemacs/init ()
  "Tests for `dotspacemacs/init'"
  (insert
   (format (concat "\n* Testing settings in dotspacemacs/init "
                   "[[file:%s::dotspacemacs/init][Show in File]]\n")
           dotspacemacs-filepath))
  (dotspacemacs||let-init-test
   (dotspacemacs/init)
   (spacemacs//test-var
    (lambda (x)
      (let ((themes '(spacemacs
                      all-the-icons
                      memacs
                      custom
                      doom
                      vim-powerline
                      vanilla)))
        (or (member x themes)
            (and (listp x)
                 (memq (car x) themes)
                 ;; TODO define a function to remove several properties at once
                 (null (spacemacs/mplist-remove (spacemacs/mplist-remove (cdr x) :separator)
                                         :separator-scale))))))
    'dotspacemacs-mode-line-theme
    (concat
     "is \'spacemacs, \'all-the-icons, \'custom, \'vim-powerline or 'vanilla "
     "or a list with `car' one of the previous values and properties one of "
     "the following: `:separator' or `:separator-scale'"))
   (spacemacs//test-var
    (lambda (x) (member x '(original cache nil)))
    'dotspacemacs-auto-save-file-location (concat "is one of \'original, "
                                                  "\'cache or nil"))
   (spacemacs//test-var
    (lambda (x) (member x '(all any current nil)))
    'dotspacemacs-highlight-delimiters "is one of \'all, \'any, \'current or nil")
   (spacemacs//test-list
    (lambda (x)
      (let ((el (or (car-safe x) x))
            (list-size (cdr-safe x)))
      (member el '(recents bookmarks projects todos agenda))))
    'dotspacemacs-startup-lists (concat "includes \'recents, "
                              "\'bookmarks, \'todos, "
                              "\'agenda or \'projects"))
   (spacemacs//test-list
    (lambda (x)
      (let ((el (or (car-safe x) x))
            (list-size (cdr-safe x)))
        (or (null list-size)(numberp list-size))))
    'dotspacemacs-startup-lists (concat "list size is a number"))
   (spacemacs//test-var 'stringp 'dotspacemacs-leader-key "is a string")
   (spacemacs//test-var 'stringp 'dotspacemacs-emacs-leader-key "is a string")
   (spacemacs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspacemacs-major-mode-leader-key "is a string or nil")
   (spacemacs//test-var
    (lambda (x) (or (null x) (stringp x)))
    'dotspacemacs-major-mode-emacs-leader-key "is a string or nil")
   (spacemacs//test-var 'stringp 'dotspacemacs-emacs-command-key "is a string")
   (insert (format
            (concat "** RESULTS: "
                    "[[file:%s::dotspacemacs/init][dotspacemacs/init]] "
                    "passed %s out of %s tests\n")
            dotspacemacs-filepath passed-tests total-tests))
   (equal passed-tests total-tests)))

(defun dotspacemacs/test-dotfile (&optional hide-buffer)
  "Test settings in dotfile for correctness.
 Return non-nil if all the tests passed."
  (interactive)
  (configuration-layer/discover-layers 'refresh-index)
  (let ((min-version "0.0"))
    ;; dotspacemacs-version not implemented yet
    ;; (if (version< dotspacemacs-version min-version)
    (if nil
        (error (format (concat "error: dotspacemacs/test-dotfile requires "
                               "dotspacemacs-version %s") min-version))
      (with-current-buffer (get-buffer-create dotspacemacs-test-results-buffer)
        (unless hide-buffer
          (switch-to-buffer-other-window dotspacemacs-test-results-buffer))
        (org-mode)
        (org-indent-mode)
        (view-mode)
        (when (bound-and-true-p flyspell-mode)
          (flyspell-mode -1))
        (let (buffer-read-only)
          (erase-buffer)
          (insert (format "* Running tests on [[file:%s][%s]] (v%s)\n"
                          dotspacemacs-filepath dotspacemacs-filepath "0.0"))
          ;; dotspacemacs-version not implemented yet
          ;; (insert (format "* Running tests on %s (v%s)\n" dotspacemacs-filepath dotspacemacs-version))
          (prog1
              ;; execute all tests no matter what
              (cl-reduce (lambda (x y)
                           (and (funcall y) x))
                         '(dotspacemacs//test-dotspacemacs/layers
                           dotspacemacs//test-dotspacemacs/init)
                         :initial-value t)
            (goto-char (point-min))))))))

(provide 'core-dotspacemacs)
