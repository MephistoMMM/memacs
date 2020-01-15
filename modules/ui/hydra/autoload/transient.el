;;; ui/hydra/autoload/transient.el -*- lexical-binding: t; -*-

(defvar doom-transient-state-color-guide nil)
(defvar doom-transient-state-title nil)

(defvar doom-transient-state-enter-hook nil
  "A list of hooks to run when enter hydra.")

(defvar doom-transient-state-exit-hook nil
  "A list of hooks to run when exit hydra.")

(defun doom/ts-run-enter-hooks ()
  "Run functions in `doom-ts-enter-hook'"
  (run-hooks 'doom-transient-state-enter-hook))

(defun doom/ts-run-exit-hooks ()
  "Run functions in `doom-ts-exit-hook'"
  (run-hooks 'doom-transient-state-exit-hook))

(defun doom/ts-func-name (name)
  "Return the name of the transient state function."
  (intern (format "doom/%S-ts" name)))

(defun doom/ts-props-var-name (name)
  "Return the name of the variable use to store the transient state properties."
  (intern (format "doom--%S-ts-props" name)))

(defun doom/ts-body-func-name (name)
  "Return the name of the transient state function."
  (intern (format "doom/%S-ts/body" name)))

(defun doom/ts-heads-name (name)
  "Return the name of the transient state heads variable which
holds the key bindings."
  (intern (format "doom/%S-ts/heads" name)))

(defun doom/ts-full-hint-toggle-name (name)
  "Return the name of the transient state full hint toggle variable which
holds the key bindings."
  (intern (format "doom--%S-ts-full-hint-toggle" name)))

(defun doom/ts-toggle-hint-func-name (name)
  "Return the name of the transient state toggle hint func which
holds the key bindings."
  (intern (format "doom/%S-ts-toggle-hint" name)))

(defun doom/ts-hint-func-name (name)
  "Return the name of the transient state hint func which
holds the key bindings."
  (intern (format "doom/%S-ts-hint" name)))

(defun doom/ts-inner-full-hint-name (name)
  "Return the name of the transient state inner full hint variable which
holds the key bindings."
  (intern (format "doom--%S-ts-full-hint" name)))

(defun doom/ts-add-bindings-name (name)
  "Return the name of the transient state add-bindings variable which
may hold the additional key bindings. The variable may be unbound."
  (intern (format "doom-%s-ts-add-bindings" name)))

(defun doom/ts-remove-bindings-name (name)
  "Return the name of the transient state remove-bindings variable which
may hold the keys to be removed. The variable may be unbound."
  (intern (format "doom-%s-ts-remove-bindings" name)))

(defun doom/ts-action-func (name action)
  (intern (format "doom/%S-ts/%S" name action)))

(defun doom/ts-adjust-bindings (bindings to-remove to-add)
  (append
   (cl-remove-if
    (lambda (bnd)
      (and (boundp to-remove)
         (listp (symbol-value to-remove))
         (member (car bnd) (symbol-value to-remove))))
    bindings)
   (when (and (boundp to-add)
            (listp (symbol-value to-add)))
     (symbol-value to-add))))

(defun doom/ts-make-doc
    (ts docstring &optional body)
  "Use `hydra' internal function to format and apply DOCSTRING."
  (let ((heads (doom/ts-heads-name ts)))
    (setq body (if body body '(nil nil :hint nil :foreign-keys nil)))
    (eval
     (hydra--format nil body docstring (symbol-value heads)))))

;;;###autoload
(defun doom/ts-register-add-bindings (name bindings)
  "Register additional BINDINGS for the transient state NAME.

BINDINGS should be a list of Hydra head definitions. See `defhydra'.

Since a transient state initializes its Hydra right after
the `doom/user-config', this function will have no
effect if called after that point."
  (declare (indent defun))
  (let ((var-name (doom/ts-add-bindings-name name)))
    (or (boundp var-name)
       (set var-name '()))
    (set var-name (append (symbol-value var-name) bindings))))

;;;###autoload
(defun doom/ts-register-remove-bindings (name keys)
  "Register KEYS to be removed from the transient state NAME.

KEYS should be a list of strings.

Since a transient state initializes its Hydra right after
the `doom/user-config', this function will have no
effect if called after that point."
  (declare (indent defun))
  (let ((var-name (doom/ts-remove-bindings-name name)))
    (or (boundp var-name)
       (set var-name '()))
    (set var-name (append (symbol-value var-name) keys))))

(defmacro ts-format-hint! (name var hint)
  "Format HINT and store the result in VAR for transient state NAME."
  (declare (indent 1))
  `(let* ((props-var ,(doom/ts-props-var-name
                      name))
         (prop-hint (cadr (assq 'hint props-var)))
         (prop-columns (cadr (assq 'columns props-var)))
         (prop-foreign-keys (cadr (assq 'foreign-keys props-var)))
         (prop-entry-sexp (cadr (assq 'entry-sexp props-var)))
         (prop-exit-sexp (cadr (assq 'exit-sexp props-var))))
    (setq ,var (doom/ts-make-doc
                ',name
                ,hint
                `(nil
                  nil
                  :hint ,prop-hint
                  :columns ,prop-columns
                  :foreign-keys ,prop-foreign-keys
                  :body-pre ,prop-entry-sexp
                  :before-exit ,prop-exit-sexp)))
    ))

(defface doom-transient-state-title-face
  `((t :inherit mode-line))
  "Face for title of transient states.")

;;;###autoload
(defmacro define-hintable-transient-state! (name &rest props)
  "Define a transient state called NAME.
NAME is a symbol.
Available PROPS:
`:on-enter SEXP'
    Evaluate SEXP when the transient state is switched on.
`:on-exit SEXP'
    Evaluate SEXP when leaving the transient state.
`:doc STRING or SEXP'
    A full docstring supported by `defhydra'.
`:brief-format FUNC
    A function return a docstring represented brief doc while hint down
`:additional-docs cons cells (VARIABLE . STRING)'
    Additional docstrings to format and store in the corresponding VARIABLE.
    This can be used to dynamically change the docstring.
`:title STRING'
    Provide a title in the header of the transient state
`:columns INTEGER'
    Automatically generate :doc with this many number of columns.
`:hint BOOLEAN'
    Whether to display hints. Default is nil.
`:foreign-keys SYMBOL'
    What to do when keys not bound in the transient state are entered. This
    can be nil (default), which means to exit the transient state, warn,
    which means to not exit but warn the user that the key is not part
    of the transient state, or run, which means to try to run the key binding
    without exiting.
`:bindings EXPRESSIONS'
    One or several EXPRESSIONS with the form
    (STRING1 SYMBOL1 DOCSTRING
                     :exit SYMBOL)
    *This macro will append a (\"?\" TOGGLE-HINT-FUNC) to bindings.*
    where:
    - STRING1 is a key to be bound to the function or key map SYMBOL1.
    - DOCSTRING is a STRING or an SEXP that evaluates to a string
    - :exit SYMBOL or SEXP, if non nil then pressing this key will
      leave the transient state (default is nil).
      Important note: due to inner working of transient-maps in Emacs
      the `:exit' keyword is evaluate *before* the actual execution
      of the bound command.
All properties supported by `doom/create-key-binding-form' can be
used.
"
  (let ((full-hint-toggle (doom/ts-full-hint-toggle-name name))
        (inner-full-hint (doom/ts-inner-full-hint-name name))
        (toggle-hint-func (doom/ts-toggle-hint-func-name name))
        (hint-func (doom/ts-hint-func-name name))
        (brief-format-func (plist-get props :brief-format)))
    `(progn
       (defvar ,full-hint-toggle nil
         "Toggle display of ts documentation.")
       (defun ,toggle-hint-func ()
         "Toggle the full hint docstring for the ts."
         (interactive)
         (setq ,full-hint-toggle (not ,full-hint-toggle)))
       (defun ,hint-func ()
         "Return a one liner string containing or full hint."
         (concat
          ,(if brief-format-func
               `(,(doom-unquote brief-format-func))
             " ")
          (if ,full-hint-toggle
              ,inner-full-hint
            (concat "  (["
                    (propertize "?" 'face 'hydra-face-red)
                    "] help)"))))
       (let ((doc ,(plist-get props :doc)))
         (define-transient-state! ,name
          :hint-is-doc t
          :dynamic-hint (,hint-func)
          ,@(plist-put props :doc "\n")
          ;; HACK add binding to "?"
          ("?" ,toggle-hint-func))
         (ts-format-hint! ,name ,inner-full-hint doc))
       ))
  )

;;;###autoload
(defmacro define-transient-state! (name &rest props)
  "Define a transient state called NAME.
NAME is a symbol.
Available PROPS:
`:on-enter SEXP'
    Evaluate SEXP when the transient state is switched on.
`:on-exit SEXP'
    Evaluate SEXP when leaving the transient state.
`:doc STRING or SEXP'
    A docstring supported by `defhydra'.
`:additional-docs cons cells (VARIABLE . STRING)'
    Additional docstrings to format and store in the corresponding VARIABLE.
    This can be used to dynamically change the docstring.
`:title STRING'
    Provide a title in the header of the transient state
`:columns INTEGER'
    Automatically generate :doc with this many number of columns.
`:hint BOOLEAN'
    Whether to display hints. Default is nil.
`:hint-is-doc BOOLEAN'
    Whether the hints act as a documentation, the only effect of this value is
    to change where the hints are displayed. If non-nil the hints are displayed
    on the same line as the `:title', otherwise they are displayed below it.
    Default is nil.
`:dynamic-hint SEXP'
    An sexp evaluating to a string for dynamic hinting.
    When provided `:hint' has no effect. Default is nil.
`:foreign-keys SYMBOL'
    What to do when keys not bound in the transient state are entered. This
    can be nil (default), which means to exit the transient state, warn,
    which means to not exit but warn the user that the key is not part
    of the transient state, or run, which means to try to run the key binding
    without exiting.
`:bindings EXPRESSIONS'
    One or several EXPRESSIONS with the form
    (STRING1 SYMBOL1 DOCSTRING
                     :exit SYMBOL)
    where:
    - STRING1 is a key to be bound to the function or key map SYMBOL1.
    - DOCSTRING is a STRING or an SEXP that evaluates to a string
    - :exit SYMBOL or SEXP, if non nil then pressing this key will
      leave the transient state (default is nil).
      Important note: due to inner working of transient-maps in Emacs
      the `:exit' keyword is evaluate *before* the actual execution
      of the bound command.
All properties supported by `doom/create-key-binding-form' can be
used."
  (declare (indent 1))
  (let* ((func (doom/ts-func-name name))
         (props-var (doom/ts-props-var-name name))
         (body-func (doom/ts-body-func-name name))
         (add-bindings (doom/ts-add-bindings-name name))
         (remove-bindings (doom/ts-remove-bindings-name name))
         (bindings (spacemacs/mplist-get-values props :bindings))
         (doc (or (plist-get props :doc) "\n"))
         (title (plist-get props :title))
         (hint-var (intern (format "%s/hint" func)))
         (columns (plist-get props :columns))
         (entry-sexp (plist-get props :on-enter))
         (exit-sexp (plist-get props :on-exit))
         (hint (plist-get props :hint))
         (hint-doc-p (plist-get props :hint-is-doc))
         (dyn-hint (plist-get props :dynamic-hint))
         ;; (additional-docs (spacemacs/mplist-get-values props :additional-docs))
         (foreign-keys (plist-get props :foreign-keys))
         (bindkeys (spacemacs//create-key-binding-form props body-func)))
    `(progn
       (defvar ,props-var nil
         ,(format (concat "Association list containing a copy of some "
                          "properties of the transient state %S. Those "
                          "properties are used in macro "
                          "`ts-format-hint!'.") name))
       (add-to-list ',props-var '(hint ,hint))
       (add-to-list ',props-var '(columns ,columns))
       (add-to-list ',props-var '(foreign-keys ,foreign-keys))
       (add-to-list ',props-var '(entry-sexp ,entry-sexp))
       (add-to-list ',props-var '(exit-sexp ,exit-sexp))
       (eval
        (append
         '(defhydra ,func
            (nil nil
               :hint ,hint
               :columns ,columns
               :foreign-keys ,foreign-keys
               ;; custom action at beginning/end of hydra body
               :body-pre (progn (doom/ts-run-enter-hooks) ,entry-sexp)
               :before-exit (progn (doom/ts-run-exit-hooks) ,exit-sexp))
            ,doc)
         (doom/ts-adjust-bindings
          ',bindings ',remove-bindings ',add-bindings)))
       (when ,title
         (let ((guide (concat "[" (propertize "KEY" 'face 'hydra-face-blue)
                              "] exits state  ["
                              (if ',foreign-keys
                                  (propertize "KEY" 'face 'hydra-face-pink)
                                (propertize "KEY" 'face 'hydra-face-red))
                              "] will not exit")))
           ;; (add-face-text-property 0 (length guide) '(:height 0.9) t guide)
           (add-face-text-property 0 (length guide) 'italic t guide)
           (setq ,hint-var
                 (list 'concat
                       (when doom-transient-state-title
                         (concat
                          (propertize
                           ,title
                           'face 'doom-transient-state-title-face)
                          (if ,hint-doc-p " " "\n"))) ,hint-var
                       ',dyn-hint
                       (when doom-transient-state-color-guide
                         (concat "\n" guide))))))
       ,@bindkeys)))


;;;###autoload
(defmacro open-ts-and-do! (name action &optional interactive-arg)
  "Create a function wapper the active"
  `(defun ,(doom/ts-action-func name action) (&rest args)
     "Open transient then do action"
     ,(if interactive-arg
          `(interactive ,interactive-arg)
        `(interactive))
     (,(doom/ts-body-func-name name))
     (apply ',action args)))
