;;; pretty-code.el --- pretty-code package
;;
;; Copyright (c) 2015-2019 Mephis Pheies
;;
;; Author: Mephis Pheies <mephistommm@gmail.com>
;; URL: https://github.com/MephistoMMM/memacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;;###autoload
(defvar +pretty-code-symbols
  '(;; org
    :name          "»"
    :src_block     "»"
    :src_block_end "«"
    ;; Functional
    :lambda        "λ"
    :def           "ƒ"
    :composition   "∘"
    :map           "↦"
    ;; Types
    :null          "∅"
    :true          "𝕋"
    :false         "𝔽"
    :int           "ℤ"
    :float         "ℝ"
    :str           "𝕊"
    :bool          "𝔹"
    ;; Flow
    :not           "￢"
    :in            "∈"
    :not-in        "∉"
    :and           "∧"
    :or            "∨"
    :for           "∀"
    :some          "∃"
    :return        "⟼"
    :yield         "⟻"
    ;; Other
    :tuple         "⨂"
    :pipe          "" ;; FIXME: find a non-private char
    :dot           "•")
  "Options plist for `set-pretty-symbols!'.

This should not contain any symbols from the Unicode Private Area! There is no
universal way of getting the correct symbol as that area varies from font to
font.")

(defvar +pretty-code-fira-code-font-name "Fira Code Symbol"
  "Name of the fira code ligature font.")

(defvar +pretty-code-fira-code-font-ligatures
  '(("www"         . #Xe100)
    ("**"          . #Xe101)
    ("***"         . #Xe102)
    ("**/"         . #Xe103)
    ("*>"          . #Xe104)
    ("*/"          . #Xe105)
    ("\\\\"        . #Xe106)
    ("\\\\\\"      . #Xe107)
    ("{-"          . #Xe108)
    ("[]"          . #Xe109)
    ("::"          . #Xe10a)
    (":::"         . #Xe10b)
    (":="          . #Xe10c)
    ("!!"          . #Xe10d)
    ("!="          . #Xe10e)
    ("!=="         . #Xe10f)
    ("-}"          . #Xe110)
    ("--"          . #Xe111)
    ("---"         . #Xe112)
    ("-->"         . #Xe113)
    ("->"          . #Xe114)
    ("->>"         . #Xe115)
    ("-<"          . #Xe116)
    ("-<<"         . #Xe117)
    ("-~"          . #Xe118)
    ("#{"          . #Xe119)
    ("#["          . #Xe11a)
    ("##"          . #Xe11b)
    ("###"         . #Xe11c)
    ("####"        . #Xe11d)
    ("#("          . #Xe11e)
    ("#?"          . #Xe11f)
    ("#_"          . #Xe120)
    ("#_("         . #Xe121)
    (".-"          . #Xe122)
    (".="          . #Xe123)
    (".."          . #Xe124)
    ("..<"         . #Xe125)
    ("..."         . #Xe126)
    ("?="          . #Xe127)
    ("??"          . #Xe128)
    (";;"          . #Xe129)
    ("/*"          . #Xe12a)
    ("/**"         . #Xe12b)
    ("/="          . #Xe12c)
    ("/=="         . #Xe12d)
    ("/>"          . #Xe12e)
    ("//"          . #Xe12f)
    ("///"         . #Xe130)
    ("&&"          . #Xe131)
    ("||"          . #Xe132)
    ("||="         . #Xe133)
    ("|="          . #Xe134)
    ("|>"          . #Xe135)
    ("^="          . #Xe136)
    ("$>"          . #Xe137)
    ("++"          . #Xe138)
    ("+++"         . #Xe139)
    ("+>"          . #Xe13a)
    ("=:="         . #Xe13b)
    ("=="          . #Xe13c)
    ("==="         . #Xe13d)
    ("==>"         . #Xe13e)
    ("=>"          . #Xe13f)
    ("=>>"         . #Xe140)
    ("<="          . #Xe141)
    ("=<<"         . #Xe142)
    ("=/="         . #Xe143)
    (">-"          . #Xe144)
    (">="          . #Xe145)
    (">=>"         . #Xe146)
    (">>"          . #Xe147)
    (">>-"         . #Xe148)
    (">>="         . #Xe149)
    (">>>"         . #Xe14a)
    ("<*"          . #Xe14b)
    ("<*>"         . #Xe14c)
    ("<|"          . #Xe14d)
    ("<|>"         . #Xe14e)
    ("<$"          . #Xe14f)
    ("<$>"         . #Xe150)
    ("<!--"        . #Xe151)
    ("<-"          . #Xe152)
    ("<--"         . #Xe153)
    ("<->"         . #Xe154)
    ("<+"          . #Xe155)
    ("<+>"         . #Xe156)
    ("<="          . #Xe157)
    ("<=="         . #Xe158)
    ("<=>"         . #Xe159)
    ("<=<"         . #Xe15a)
    ("<>"          . #Xe15b)
    ("<<"          . #Xe15c)
    ("<<-"         . #Xe15d)
    ("<<="         . #Xe15e)
    ("<<<"         . #Xe15f)
    ("<~"          . #Xe160)
    ("<~~"         . #Xe161)
    ("</"          . #Xe162)
    ("</>"         . #Xe163)
    ("~@"          . #Xe164)
    ("~-"          . #Xe165)
    ("~="          . #Xe166)
    ("~>"          . #Xe167)
    ("~~"          . #Xe168)
    ("~~>"         . #Xe169)
    ("%%"          . #Xe16a)))

;;;###autoload
(defun +pretty-code|setup-fira-ligatures ()
  (set-fontset-font t '(#Xe100 . #Xe16f) +pretty-code-fira-code-font-name)
  (setq-default prettify-symbols-alist
                (append prettify-symbols-alist
                        (mapcar #'+pretty-code--correct-symbol-bounds
                                +pretty-code-fira-code-font-ligatures))))

(defvar +pretty-code-enabled-modes t
  "List of major modes in which `prettify-symbols-mode' should be enabled.
If t, enable it everywhere. If the first element is 'not, enable it in any mode
besides what is listed.")

;; When you get to the right edge, it goes back to how it normally prints
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defun +pretty-code|init-pretty-symbols ()
  "Enable `prettify-symbols-mode'.

If in fundamental-mode, or a mode derived from special, comint, eshell or term
modes, this function does nothing.

Otherwise it builds `prettify-code-symbols-alist' according to
`+pretty-code-symbols-alist' for the current major-mode."
  (unless (or (eq major-mode 'fundamental-mode)
              (eq (get major-mode 'mode-class) 'special)
              (derived-mode-p 'comint-mode 'eshell-mode 'term-mode))
    (when (or (eq +pretty-code-enabled-modes t)
              (if (eq (car +pretty-code-enabled-modes) 'not)
                  (not (memq major-mode (cdr +pretty-code-enabled-modes)))
                (memq major-mode +pretty-code-enabled-modes)))
      (setq prettify-symbols-alist
            (append (cdr (assq major-mode +pretty-code-symbols-alist))
                    (default-value 'prettify-symbols-alist)))
      (when prettify-symbols-mode
        (prettify-symbols-mode -1))
      (prettify-symbols-mode +1))))

(add-hook 'after-change-major-mode-hook #'+pretty-code|init-pretty-symbols)

(defun doom-enlist (exp)
  "Return EXP wrapped in a list, or as-is if already a list."
  (declare (pure t) (side-effect-free t))
  (if (listp exp) exp (list exp)))

;;;###autoload
(defvar +pretty-code-symbols-alist '((t))
  "An alist containing a mapping of major modes to its value for
`prettify-symbols-alist'.")

;;;###autodef
(defun +pretty-code--correct-symbol-bounds (ligature-alist)
  "Prepend non-breaking spaces to a ligature.

This way `compose-region' (called by `prettify-symbols-mode') will use the
correct width of the symbols instead of the width measured by `char-width'."
  (let ((len (length (car ligature-alist)))
        (acc (list   (cdr ligature-alist))))
    (while (> len 1)
      (setq acc (cons #X00a0 (cons '(Br . Bl) acc))
            len (1- len)))
    (cons (car ligature-alist) acc)))

;;;###autodef
(defun set-pretty-symbols! (modes &rest plist)
  "Associates string patterns with icons in certain major-modes.

  MODES is a major mode symbol or a list of them.
  PLIST is a property list whose keys must match keys in `+pretty-code-symbols',
and whose values are strings representing the text to be replaced with that
symbol. If the car of PLIST is nil, then unset any pretty symbols previously
defined for MODES.

The following properties are special:

  :alist ALIST
    Appends ALIST to `prettify-symbols-alist' literally, without mapping text to
    `+pretty-code-symbols'.
  :merge BOOL
    If non-nil, merge with previously defined `prettify-symbols-alist',
    otherwise overwrite it.

For example, the rule for emacs-lisp-mode is very simple:

  (set-pretty-symbols! 'emacs-lisp-mode
    :lambda \"lambda\")

This will replace any instances of \"lambda\" in emacs-lisp-mode with the symbol
assicated with :lambda in `+pretty-code-symbols'.

Pretty symbols can be unset for emacs-lisp-mode with:

  (set-pretty-symbols! 'emacs-lisp-mode nil)"
  (declare (indent defun))
  (if (null (car-safe plist))
      (dolist (mode (doom-enlist modes))
        (delq (assq mode +pretty-code-symbols-alist)
              +pretty-code-symbols-alist))
    (let (results merge key)
      (while plist
        (pcase (setq key (pop plist))
          (:merge (setq merge (pop plist)))
          (:alist (setq results (append (pop plist) results)))
          (_
           (when-let* ((char (plist-get +pretty-code-symbols key)))
             (push (cons (pop plist) char) results)))))
      (dolist (mode (doom-enlist modes))
        (unless merge
          (delq (assq mode +pretty-code-symbols-alist)
                +pretty-code-symbols-alist))
        (push (cons mode results) +pretty-code-symbols-alist)))))

(provide 'pretty-code)

;;; pretty-code.el ends here
