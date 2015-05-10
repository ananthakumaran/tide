;;; typescript.el --- Major mode for editing typescript

;; -----------------------------------------------------------------------------------
;;     TypeScript support for Emacs
;;     Unmodified original sourve available at http://www.karllandstrom.se/downloads/emacs/javascript.el
;;     Copyright (c) 2008 Free Software Foundation
;;     Portions Copyright (C) Microsoft Open Technologies, Inc. All rights reserved.
;;
;;     This program is free software: you can redistribute it and/or modify
;;     it under the terms of the GNU General Public License as published by
;;     the Free Software Foundation, either version 3 of the License, or
;;     (at your option) any later version.
;;
;;     This program is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU General Public License for more details.
;;
;;     You should have received a copy of the GNU General Public License
;;     along with this program.  If not, see <http://www.gnu.org/licenses/>.
;; -------------------------------------------------------------------------------------------

;;; Commentary:

;; This is based on Karl Landstrom's barebones typescript-mode. This
;; is much more robust and works with cc-mode's comment filling
;; (mostly).
;; The modifications to the original javascript.el mode mainly consisted in
;; replacing "javascript" with "typescript"
;;
;; The main features of this typescript mode are syntactic
;; highlighting (enabled with `font-lock-mode' or
;; `global-font-lock-mode'), automatic indentation and filling of
;; comments, C preprocessor fontification, and MozRepl integration.
;;
;;
;; General Remarks:
;;
;; XXX: This mode assumes that block comments are not nested inside block
;; XXX: comments
;;
;; Exported names start with "typescript-"; private names start with
;; "typescript--".

;;; Code:

(eval-and-compile
  (require 'cc-mode)
  (require 'font-lock)
  (require 'newcomment)
  (require 'imenu)
  (require 'etags)
  (require 'thingatpt)
  (require 'ido)
  (require 'comint)
  (require 'easymenu)
  (require 'moz nil t)
  (require 'json nil t))

(eval-when-compile
  (require 'cl))

(defvar inferior-moz-buffer)
(defvar moz-repl-name)
(defvar ido-cur-list)
(declare-function ido-mode "ido")
(declare-function inferior-moz-process "ext:mozrepl" ())

;;; Constants

(defconst typescript--name-start-re "[a-zA-Z_$]"
  "Regexp matching the start of a typescript identifier, without grouping.")

(defconst typescript--stmt-delim-chars "^;{}?:")

(defconst typescript--name-re (concat typescript--name-start-re
                              "\\(?:\\s_\\|\\sw\\)*")
  "Regexp matching a typescript identifier, without grouping.")

(defconst typescript--objfield-re (concat typescript--name-re ":")
  "Regexp matching the start of a typescript object field.")

(defconst typescript--dotted-name-re
  (concat typescript--name-re "\\(?:\\." typescript--name-re "\\)*")
  "Regexp matching a dot-separated sequence of typescript names.")

(defconst typescript--cpp-name-re typescript--name-re
  "Regexp matching a C preprocessor name.")

(defconst typescript--opt-cpp-start "^\\s-*#\\s-*\\([[:alnum:]]+\\)"
  "Regexp matching the prefix of a cpp directive.
This includes the directive name, or nil in languages without
preprocessor support.  The first submatch surrounds the directive
name.")

(defconst typescript--plain-method-re
  (concat "^\\s-*?\\(" typescript--dotted-name-re "\\)\\.prototype"
          "\\.\\(" typescript--name-re "\\)\\s-*?=\\s-*?\\(function\\)\\_>")
  "Regexp matching an explicit typescript prototype \"method\" declaration.
Group 1 is a (possibly-dotted) class name, group 2 is a method name,
and group 3 is the 'function' keyword.")

(defconst typescript--plain-class-re
  (concat "^\\s-*\\(" typescript--dotted-name-re "\\)\\.prototype"
          "\\s-*=\\s-*{")
  "Regexp matching a typescript explicit prototype \"class\" declaration.
An example of this is \"Class.prototype = { method1: ...}\".")

;; var NewClass = BaseClass.extend(
(defconst typescript--mp-class-decl-re
  (concat "^\\s-*var\\s-+"
          "\\(" typescript--name-re "\\)"
          "\\s-*=\\s-*"
          "\\(" typescript--dotted-name-re
          "\\)\\.extend\\(?:Final\\)?\\s-*(\\s-*{?\\s-*$"))

;; var NewClass = Class.create()
(defconst typescript--prototype-obsolete-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" typescript--dotted-name-re "\\)"
          "\\s-*=\\s-*Class\\.create()"))

(defconst typescript--prototype-objextend-class-decl-re-1
  (concat "^\\s-*Object\\.extend\\s-*("
          "\\(" typescript--dotted-name-re "\\)"
          "\\s-*,\\s-*{"))

(defconst typescript--prototype-objextend-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" typescript--dotted-name-re "\\)"
          "\\s-*=\\s-*Object\\.extend\\s-*\("))

;; var NewClass = Class.create({
(defconst typescript--prototype-class-decl-re
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" typescript--name-re "\\)"
          "\\s-*=\\s-*Class\\.create\\s-*(\\s-*"
          "\\(?:\\(" typescript--dotted-name-re "\\)\\s-*,\\s-*\\)?{?"))

;; Parent class name(s) (yes, multiple inheritance in typescript) are
;; matched with dedicated font-lock matchers
(defconst typescript--dojo-class-decl-re
  (concat "^\\s-*dojo\\.declare\\s-*(\"\\(" typescript--dotted-name-re "\\)"))

(defconst typescript--exttypescript-class-decl-re-1
  (concat "^\\s-*Ext\\.extend\\s-*("
          "\\s-*\\(" typescript--dotted-name-re "\\)"
          "\\s-*,\\s-*\\(" typescript--dotted-name-re "\\)")
  "Regexp matching an ExtTYPESCRIPT class declaration (style 1).")

(defconst typescript--exttypescript-class-decl-re-2
  (concat "^\\s-*\\(?:var\\s-+\\)?"
          "\\(" typescript--name-re "\\)"
          "\\s-*=\\s-*Ext\\.extend\\s-*(\\s-*"
          "\\(" typescript--dotted-name-re "\\)")
  "Regexp matching an ExtTYPESCRIPT class declaration (style 2).")

(defconst typescript--mochikit-class-re
  (concat "^\\s-*MochiKit\\.Base\\.update\\s-*(\\s-*"
          "\\(" typescript--dotted-name-re "\\)")
  "Regexp matching a MochiKit class declaration.")

(defconst typescript--dummy-class-style
  '(:name "[Automatically Generated Class]"))

(defconst typescript--class-styles
  `((:name            "Plain"
     :class-decl      ,typescript--plain-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       typescript)

    (:name            "MochiKit"
     :class-decl      ,typescript--mochikit-class-re
     :prototype       t
     :contexts        (toplevel)
     :framework       mochikit)

    (:name            "Prototype (Obsolete)"
     :class-decl      ,typescript--prototype-obsolete-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Modern)"
     :class-decl      ,typescript--prototype-class-decl-re
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend)"
     :class-decl      ,typescript--prototype-objextend-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Prototype (Object.extend) 2"
     :class-decl      ,typescript--prototype-objextend-class-decl-re-2
     :prototype       t
     :contexts        (toplevel)
     :framework       prototype)

    (:name            "Dojo"
     :class-decl      ,typescript--dojo-class-decl-re
     :contexts        (toplevel)
     :framework       dojo)

    (:name            "ExtTYPESCRIPT (style 1)"
     :class-decl      ,typescript--exttypescript-class-decl-re-1
     :prototype       t
     :contexts        (toplevel)
     :framework       exttypescript)

    (:name            "ExtTYPESCRIPT (style 2)"
     :class-decl      ,typescript--exttypescript-class-decl-re-2
     :contexts        (toplevel)
     :framework       exttypescript)

    (:name            "Merrill Press"
     :class-decl      ,typescript--mp-class-decl-re
     :contexts        (toplevel)
     :framework       merrillpress))

  "List of typescript class definition styles.

A class definition style is a plist with the following keys:

:name is a human-readable name of the class type

:class-decl is a regular expression giving the start of the
class.  Its first group must match the name of its class.  If there
is a parent class, the second group should match, and it should be
the name of the class.

If :prototype is present and non-nil, the parser will merge
declarations for this constructs with others at the same lexical
level that have the same name.  Otherwise, multiple definitions
will create multiple top-level entries.  Don't use :prototype
unnecessarily: it has an associated cost in performance.

If :strip-prototype is present and non-nil, then if the class
name as matched contains
")

(defconst typescript--available-frameworks
  (loop with available-frameworks
        for style in typescript--class-styles
        for framework = (plist-get style :framework)
        unless (memq framework available-frameworks)
        collect framework into available-frameworks
        finally return available-frameworks)
  "List of available typescript frameworks symbols.")

(defconst typescript--function-heading-1-re
  (concat
   "^\\s-*function\\s-+\\(" typescript--name-re "\\)")
  "Regexp matching the start of a typescript function header.
Match group 1 is the name of the function.")

(defconst typescript--function-heading-2-re
  (concat
   "^\\s-*\\(" typescript--name-re "\\)\\s-*:\\s-*function\\_>")
  "Regexp matching the start of a function entry in an associative array.
Match group 1 is the name of the function.")

(defconst typescript--function-heading-3-re
  (concat
   "^\\s-*\\(?:var\\s-+\\)?\\(" typescript--dotted-name-re "\\)"
   "\\s-*=\\s-*function\\_>")
  "Regexp matching a line in the typescript form \"var MUMBLE = function\".
Match group 1 is MUMBLE.")

(defconst typescript--macro-decl-re
  (concat "^\\s-*#\\s-*define\\s-+\\(" typescript--cpp-name-re "\\)\\s-*(")
  "Regexp matching a CPP macro definition, up to the opening parenthesis.
Match group 1 is the name of the macro.")

(defun typescript--regexp-opt-symbol (list)
  "Like `regexp-opt', but surround the result with `\\\\_<' and `\\\\_>'."
  (concat "\\_<" (regexp-opt list t) "\\_>"))

(defconst typescript--keyword-re
  (typescript--regexp-opt-symbol
   '("any" "bool" "boolean" "break" "case" "catch" "class" "constructor"
     "continue" "declare" "default" "delete" "do" "else"
     "enum" "export" "extends" "extern" "false" "finally" "for"
     "function" "goto" "if" "implements" "import" "in"
     "instanceof" "interface" "module" "new" "null" "number"
      "private" "public" "return" "static" "string"
     "super" "switch"  "this" "throw" "true"
     "try" "typeof" "var" "void"
     "while" ))
  "Regexp matching any typescript keyword.")

(defconst typescript--basic-type-re
  (typescript--regexp-opt-symbol
   '("bool" "boolean" "string" "number" "any" "void"))
  "Regular expression matching any predefined type in typescript.")

(defconst typescript--constant-re
  (typescript--regexp-opt-symbol '("false" "null" "undefined"
                                 "Infinity" "NaN"
                                 "true" "arguments" "this"))
  "Regular expression matching any future reserved words in typescript.")


(defconst typescript--font-lock-keywords-1
  (list
   "\\_<import\\_>"
   (list typescript--function-heading-1-re 1 font-lock-function-name-face)
   (list typescript--function-heading-2-re 1 font-lock-function-name-face))
  "Level one font lock keywords for `typescript-mode'.")

(defconst typescript--font-lock-keywords-2
  (append typescript--font-lock-keywords-1
          (list (list typescript--keyword-re 1 font-lock-keyword-face)
                (list "\\_<for\\_>"
                      "\\s-+\\(each\\)\\_>" nil nil
                      (list 1 'font-lock-keyword-face))
                (cons typescript--basic-type-re font-lock-type-face)
                (cons typescript--constant-re font-lock-constant-face)))
  "Level two font lock keywords for `typescript-mode'.")

;; typescript--pitem is the basic building block of the lexical
;; database. When one refers to a real part of the buffer, the region
;; of text to which it refers is split into a conceptual header and
;; body. Consider the (very short) block described by a hypothetical
;; typescript--pitem:
;;
;;   function foo(a,b,c) { return 42; }
;;   ^                    ^            ^
;;   |                    |            |
;;   +- h-begin           +- h-end     +- b-end
;;
;; (Remember that these are buffer positions, and therefore point
;; between characters, not at them. An arrow drawn to a character
;; indicates the corresponding position is between that character and
;; the one immediately preceding it.)
;;
;; The header is the region of text [h-begin, h-end], and is
;; the text needed to unambiguously recognize the start of the
;; construct. If the entire header is not present, the construct is
;; not recognized at all. No other pitems may be nested inside the
;; header.
;;
;; The body is the region [h-end, b-end]. It may contain nested
;; typescript--pitem instances. The body of a pitem may be empty: in
;; that case, b-end is equal to header-end.
;;
;; The three points obey the following relationship:
;;
;;   h-begin < h-end <= b-end
;;
;; We put a text property in the buffer on the character *before*
;; h-end, and if we see it, on the character *before* b-end.
;;
;; The text property for h-end, typescript--pstate, is actually a list
;; of all typescript--pitem instances open after the marked character.
;;
;; The text property for b-end, typescript--pend, is simply the
;; typescript--pitem that ends after the marked character. (Because
;; pitems always end when the paren-depth drops below a critical
;; value, and because we can only drop one level per character, only
;; one pitem may end at a given character.)
;;
;; In the structure below, we only store h-begin and (sometimes)
;; b-end. We can trivially and quickly find h-end by going to h-begin
;; and searching for an typescript--pstate text property. Since no other
;; typescript--pitem instances can be nested inside the header of a
;; pitem, the location after the character with this text property
;; must be h-end.
;;
;; typescript--pitem instances are never modified (with the exception
;; of the b-end field). Instead, modified copies are added at subseqnce parse points.
;; (The exception for b-end and its caveats is described below.)
;;

(defstruct (typescript--pitem (:type list))
  ;; IMPORTANT: Do not alter the position of fields within the list.
  ;; Various bits of code depend on their positions, particularly
  ;; anything that manipulates the list of children.

  ;; List of children inside this pitem's body
  (children nil :read-only t)

  ;; When we reach this paren depth after h-end, the pitem ends
  (paren-depth nil :read-only t)

  ;; Symbol or class-style plist if this is a class
  (type nil :read-only t)

  ;; See above
  (h-begin nil :read-only t)

  ;; List of strings giving the parts of the name of this pitem (e.g.,
  ;; '("MyClass" "myMethod"), or t if this pitem is anonymous
  (name nil :read-only t)

  ;; THIS FIELD IS MUTATED, and its value is shared by all copies of
  ;; this pitem: when we copy-and-modify pitem instances, we share
  ;; their tail structures, so all the copies actually have the same
  ;; terminating cons cell. We modify that shared cons cell directly.
  ;;
  ;; The field value is either a number (buffer location) or nil if
  ;; unknown.
  ;;
  ;; If the field's value is greater than `typescript--cache-end', the
  ;; value is stale and must be treated as if it were nil. Conversely,
  ;; if this field is nil, it is guaranteed that this pitem is open up
  ;; to at least `typescript--cache-end'. (This property is handy when
  ;; computing whether we're inside a given pitem.)
  ;;
  (b-end nil))

;; The pitem we start parsing with.
(defconst typescript--initial-pitem
  (make-typescript--pitem
   :paren-depth most-negative-fixnum
   :type 'toplevel))

;;; User Customization

(defgroup typescript nil
  "Customization variables for typescript mode."
  :tag "typescript"
  :group 'languages)

(defcustom typescript-indent-level 4
  "Number of spaces for each indentation step in `typescript-mode'."
  :type 'integer
  :group 'typescript)

(defcustom typescript-expr-indent-offset 0
  "Number of additional spaces used for indentation of continued expressions.
The value must be no less than minus `typescript-indent-level'."
  :type 'integer
  :group 'typescript)

(defcustom typescript-auto-indent-flag t
  "Whether to automatically indent when typing punctuation characters.
If non-nil, the characters {}();,: also indent the current line
in typescript mode."
  :type 'boolean
  :group 'typescript)

(defcustom typescript-flat-functions nil
  "Treat nested functions as top-level functions in `typescript-mode'.
This applies to function movement, marking, and so on."
  :type 'boolean
  :group 'typescript)

(defcustom typescript-comment-lineup-func #'c-lineup-C-comments
  "Lineup function for `cc-mode-style', for C comments in `typescript-mode'."
  :type 'function
  :group 'typescript)

(defcustom typescript-enabled-frameworks typescript--available-frameworks
  "Frameworks recognized by `typescript-mode'.
To improve performance, you may turn off some frameworks you
seldom use, either globally or on a per-buffer basis."
  :type (cons 'set (mapcar (lambda (x)
                             (list 'const x))
                           typescript--available-frameworks))
  :group 'typescript)

(defcustom typescript-typescript-switch-tabs
  (and (memq system-type '(darwin)) t)
  "Whether `typescript-mode' should display tabs while selecting them.
This is useful only if the windowing system has a good mechanism
for preventing Firefox from stealing the keyboard focus."
  :type 'boolean
  :group 'typescript)

(defcustom typescript-typescript-tmpdir
  "~/.emacs.d/typescript/typescript"
  "Temporary directory used by `typescript-mode' to communicate with Mozilla.
This directory must be readable and writable by both Mozilla and
Emacs."
  :type 'directory
  :group 'typescript)

(defcustom typescript-typescript-timeout 5
  "Reply timeout for executing commands in Mozilla via `typescript-mode'.
The value is given in seconds.  Increase this value if you are
getting timeout messages."
  :type 'integer
  :group 'typescript)

(defcustom typescript-mode-hook nil
  "*Hook called by `typescript-mode'."
  :type 'hook
  :group 'typescript)

;;; KeyMap

(defvar typescript-mode-map
  (let ((keymap (make-sparse-keymap)))
    (mapc (lambda (key)
	    (define-key keymap key #'typescript-insert-and-indent))
	  '("{" "}" "(" ")" ":" ";" ","))
    (define-key keymap [(control ?c) (meta ?:)] #'typescript-eval)
    (define-key keymap [(control ?c) (control ?j)] #'typescript-set-typescript-context)
    (define-key keymap [(control meta ?x)] #'typescript-eval-defun)
    (define-key keymap [(meta ?.)] #'typescript-find-symbol)
    (easy-menu-define nil keymap "typescript Menu"
      '("typescript"
        ["Select new Mozilla context…" typescript-set-typescript-context
         (fboundp #'inferior-moz-process)]
        ["Evaluate expression in Mozilla context…" typescript-eval
         (fboundp #'inferior-moz-process)]
        ["Send current function to Mozilla…" typescript-eval-defun
         (fboundp #'inferior-moz-process)]))
    keymap)
  "Keymap for `typescript-mode'.")

(defun typescript-insert-and-indent (key)
  "Run the command bound to KEY, and indent if necessary.
Indentation does not take place if point is in a string or
comment."
  (interactive (list (this-command-keys)))
  (call-interactively (lookup-key (current-global-map) key))
  (let ((syntax (save-restriction (widen) (syntax-ppss))))
    (when (or (and (not (nth 8 syntax))
                   typescript-auto-indent-flag)
              (and (nth 4 syntax)
                   (eq (current-column)
                       (1+ (current-indentation)))))
      (indent-according-to-mode))))


;;; Syntax table and parsing

(defvar typescript-mode-syntax-table
  (let ((table (make-syntax-table)))
    (c-populate-syntax-table table)
    (modify-syntax-entry ?$ "_" table)
    table)
  "Syntax table for `typescript-mode'.")

(defvar typescript--quick-match-re nil
  "Autogenerated regexp used by `typescript-mode' to match buffer constructs.")

(defvar typescript--quick-match-re-func nil
  "Autogenerated regexp used by `typescript-mode' to match constructs and functions.")

(make-variable-buffer-local 'typescript--quick-match-re)
(make-variable-buffer-local 'typescript--quick-match-re-func)

(defvar typescript--cache-end 1
  "Last valid buffer position for the `typescript-mode' function cache.")
(make-variable-buffer-local 'typescript--cache-end)

(defvar typescript--last-parse-pos nil
  "Latest parse position reached by `typescript--ensure-cache'.")
(make-variable-buffer-local 'typescript--last-parse-pos)

(defvar typescript--state-at-last-parse-pos nil
  "Parse state at `typescript--last-parse-pos'.")
(make-variable-buffer-local 'typescript--state-at-last-parse-pos)

(defun typescript--flatten-list (list)
  (loop for item in list
        nconc (cond ((consp item)
                     (typescript--flatten-list item))
                    (item (list item)))))

(defun typescript--maybe-join (prefix separator suffix &rest list)
  "Helper function for `typescript--update-quick-match-re'.
If LIST contains any element that is not nil, return its non-nil
elements, separated by SEPARATOR, prefixed by PREFIX, and ended
with SUFFIX as with `concat'.  Otherwise, if LIST is empty, return
nil.  If any element in LIST is itself a list, flatten that
element."
  (setq list (typescript--flatten-list list))
  (when list
    (concat prefix (mapconcat #'identity list separator) suffix)))

(defun typescript--update-quick-match-re ()
  "Internal function used by `typescript-mode' for caching buffer constructs.
This updates `typescript--quick-match-re', based on the current set of
enabled frameworks."
  (setq typescript--quick-match-re
        (typescript--maybe-join
         "^[ \t]*\\(?:" "\\|" "\\)"

         ;; #define mumble
         "#define[ \t]+[a-zA-Z_]"

         (when (memq 'exttypescript typescript-enabled-frameworks)
           "Ext\\.extend")

         (when (memq 'prototype typescript-enabled-frameworks)
           "Object\\.extend")

          ;; var mumble = THING (
         (typescript--maybe-join
          "\\(?:var[ \t]+\\)?[a-zA-Z_$0-9.]+[ \t]*=[ \t]*\\(?:"
          "\\|"
          "\\)[ \t]*\("

          (when (memq 'prototype typescript-enabled-frameworks)
                    "Class\\.create")

          (when (memq 'exttypescript typescript-enabled-frameworks)
            "Ext\\.extend")

          (when (memq 'merrillpress typescript-enabled-frameworks)
            "[a-zA-Z_$0-9]+\\.extend\\(?:Final\\)?"))

         (when (memq 'dojo typescript-enabled-frameworks)
           "dojo\\.declare[ \t]*\(")

         (when (memq 'mochikit typescript-enabled-frameworks)
           "MochiKit\\.Base\\.update[ \t]*\(")

         ;; mumble.prototypeTHING
         (typescript--maybe-join
          "[a-zA-Z_$0-9.]+\\.prototype\\(?:" "\\|" "\\)"

          (when (memq 'typescript typescript-enabled-frameworks)
            '( ;; foo.prototype.bar = function(
              "\\.[a-zA-Z_$0-9]+[ \t]*=[ \t]*function[ \t]*\("

              ;; mumble.prototype = {
              "[ \t]*=[ \t]*{")))))

  (setq typescript--quick-match-re-func
        (concat "function\\|" typescript--quick-match-re)))

(defun typescript--forward-text-property (propname)
  "Move over the next value of PROPNAME in the buffer.
If found, return that value and leave point after the character
having that value; otherwise, return nil and leave point at EOB."
  (let ((next-value (get-text-property (point) propname)))
    (if next-value
        (forward-char)

      (goto-char (next-single-property-change
                  (point) propname nil (point-max)))
      (unless (eobp)
        (setq next-value (get-text-property (point) propname))
        (forward-char)))

    next-value))

(defun typescript--backward-text-property (propname)
  "Move over the previous value of PROPNAME in the buffer.
If found, return that value and leave point just before the
character that has that value, otherwise return nil and leave
point at BOB."
    (unless (bobp)
      (let ((prev-value (get-text-property (1- (point)) propname)))
        (if prev-value
            (backward-char)

          (goto-char (previous-single-property-change
                      (point) propname nil (point-min)))

          (unless (bobp)
            (backward-char)
            (setq prev-value (get-text-property (point) propname))))

        prev-value)))

(defsubst typescript--forward-pstate ()
  (typescript--forward-text-property 'typescript--pstate))

(defsubst typescript--backward-pstate ()
  (typescript--backward-text-property 'typescript--pstate))

(defun typescript--pitem-goto-h-end (pitem)
  (goto-char (typescript--pitem-h-begin pitem))
  (typescript--forward-pstate))

(defun typescript--re-search-forward-inner (regexp &optional bound count)
  "Helper function for `typescript--re-search-forward'."
  (let ((parse)
        str-terminator
        (orig-macro-end (save-excursion
                          (when (typescript--beginning-of-macro)
                            (c-end-of-macro)
                            (point)))))
    (while (> count 0)
      (re-search-forward regexp bound)
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-forward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (save-excursion (end-of-line) (point)) t))
            ((nth 7 parse)
             (forward-line))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?\/) (eq (char-after) ?\*)))
             (re-search-forward "\\*/"))
            ((and (not (and orig-macro-end
                            (<= (point) orig-macro-end)))
                  (typescript--beginning-of-macro))
             (c-end-of-macro))
            (t
             (setq count (1- count))))))
  (point))


(defun typescript--re-search-forward (regexp &optional bound noerror count)
  "Search forward, ignoring strings, cpp macros, and comments.
This function invokes `re-search-forward', but treats the buffer
as if strings, cpp macros, and comments have been removed.

If invoked while inside a macro, it treats the contents of the
macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(typescript--re-search-forward-inner regexp bound 1))
               ((< count 0)
                '(typescript--re-search-backward-inner regexp bound (- count)))
               ((> count 0)
                '(typescript--re-search-forward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))


(defun typescript--re-search-backward-inner (regexp &optional bound count)
  "Auxiliary function for `typescript--re-search-backward'."
  (let ((parse)
        str-terminator
        (orig-macro-start
         (save-excursion
           (and (typescript--beginning-of-macro)
                (point)))))
    (while (> count 0)
      (re-search-backward regexp bound)
      (when (and (> (point) (point-min))
                 (save-excursion (backward-char) (looking-at "/[/*]")))
        (forward-char))
      (setq parse (syntax-ppss))
      (cond ((setq str-terminator (nth 3 parse))
             (when (eq str-terminator t)
               (setq str-terminator ?/))
             (re-search-backward
              (concat "\\([^\\]\\|^\\)" (string str-terminator))
              (save-excursion (beginning-of-line) (point)) t))
            ((nth 7 parse)
             (goto-char (nth 8 parse)))
            ((or (nth 4 parse)
                 (and (eq (char-before) ?/) (eq (char-after) ?*)))
             (re-search-backward "/\\*"))
            ((and (not (and orig-macro-start
                            (>= (point) orig-macro-start)))
                  (typescript--beginning-of-macro)))
            (t
             (setq count (1- count))))))
  (point))


(defun typescript--re-search-backward (regexp &optional bound noerror count)
  "Search backward, ignoring strings, preprocessor macros, and comments.

This function invokes `re-search-backward' but treats the buffer
as if strings, preprocessor macros, and comments have been
removed.

If invoked while inside a macro, treat the macro as normal text."
  (let ((saved-point (point))
        (search-expr
         (cond ((null count)
                '(typescript--re-search-backward-inner regexp bound 1))
               ((< count 0)
                '(typescript--re-search-forward-inner regexp bound (- count)))
               ((> count 0)
                '(typescript--re-search-backward-inner regexp bound count)))))
    (condition-case err
        (eval search-expr)
      (search-failed
       (goto-char saved-point)
       (unless noerror
         (error (error-message-string err)))))))

(defun typescript--forward-expression ()
  "Move forward over a whole typescript expression.
This function doesn't move over expressions continued across
lines."
  (loop
   ;; non-continued case; simplistic, but good enough?
   do (loop until (or (eolp)
                      (progn
                        (forward-comment most-positive-fixnum)
                        (memq (char-after) '(?\, ?\; ?\] ?\) ?\}))))
            do (forward-sexp))

   while (and (eq (char-after) ?\n)
              (save-excursion
                (forward-char)
                (typescript--continued-expression-p)))))

(defun typescript--forward-function-decl ()
  "Move forward over a typescript function declaration.
This puts point at the 'function' keyword.

If this is a syntactically-correct non-expression function,
return the name of the function, or t if the name could not be
determined.  Otherwise, return nil."
  (assert (looking-at "\\_<function\\_>"))
  (let ((name t))
    (forward-word)
    (forward-comment most-positive-fixnum)
    (when (looking-at typescript--name-re)
      (setq name (match-string-no-properties 0))
      (goto-char (match-end 0)))
    (forward-comment most-positive-fixnum)
    (and (eq (char-after) ?\( )
         (ignore-errors (forward-list) t)
         (progn (forward-comment most-positive-fixnum)
                (and (eq (char-after) ?{)
                     name)))))

(defun typescript--function-prologue-beginning (&optional pos)
  "Return the start of the typescript function prologue containing POS.
A function prologue is everything from start of the definition up
to and including the opening brace.  POS defaults to point.
If POS is not in a function prologue, return nil."
  (let (prologue-begin)
    (save-excursion
      (if pos
          (goto-char pos)
        (setq pos (point)))

      (when (save-excursion
              (forward-line 0)
              (or (looking-at typescript--function-heading-2-re)
                  (looking-at typescript--function-heading-3-re)))

        (setq prologue-begin (match-beginning 1))
        (when (<= prologue-begin pos)
          (goto-char (match-end 0))))

      (skip-syntax-backward "w_")
      (and (or (looking-at "\\_<function\\_>")
               (typescript--re-search-backward "\\_<function\\_>" nil t))

           (save-match-data (goto-char (match-beginning 0))
                            (typescript--forward-function-decl))

           (<= pos (point))
           (or prologue-begin (match-beginning 0))))))

(defun typescript--beginning-of-defun-raw ()
  "Helper function for `typescript-beginning-of-defun'.
Go to previous defun-beginning and return the parse state for it,
or nil if we went all the way back to bob and don't find
anything."
  (typescript--ensure-cache)
  (let (pstate)
    (while (and (setq pstate (typescript--backward-pstate))
                (not (eq 'function (typescript--pitem-type (car pstate))))))
    (and (not (bobp)) pstate)))

(defun typescript--pstate-is-toplevel-defun (pstate)
  "Helper function for `typescript--beginning-of-defun-nested'.
If PSTATE represents a non-empty top-level defun, return the
top-most pitem.  Otherwise, return nil."
  (loop for pitem in pstate
        with func-depth = 0
        with func-pitem
        if (eq 'function (typescript--pitem-type pitem))
        do (incf func-depth)
        and do (setq func-pitem pitem)
        finally return (if (eq func-depth 1) func-pitem)))

(defun typescript--beginning-of-defun-nested ()
  "Helper function for `typescript--beginning-of-defun'.
Return the pitem of the function we went to the beginning of."
  (or
   ;; Look for the smallest function that encloses point...
   (loop for pitem in (typescript--parse-state-at-point)
         if (and (eq 'function (typescript--pitem-type pitem))
                 (typescript--inside-pitem-p pitem))
         do (goto-char (typescript--pitem-h-begin pitem))
         and return pitem)

   ;; ...and if that isn't found, look for the previous top-level
   ;; defun
   (loop for pstate = (typescript--backward-pstate)
         while pstate
         if (typescript--pstate-is-toplevel-defun pstate)
         do (goto-char (typescript--pitem-h-begin it))
         and return it)))

(defun typescript--beginning-of-defun-flat ()
  "Helper function for `typescript-beginning-of-defun'."
  (let ((pstate (typescript--beginning-of-defun-raw)))
    (when pstate
      (goto-char (typescript--pitem-h-begin (car pstate))))))

(defun typescript-beginning-of-defun (&optional arg)
  "Value of `beginning-of-defun-function' for `typescript-mode'."
  (setq arg (or arg 1))
  (while (and (not (eobp)) (< arg 0))
    (incf arg)
    (when (and (not typescript-flat-functions)
               (or (eq (typescript-syntactic-context) 'function)
                   (typescript--function-prologue-beginning)))
      (typescript-end-of-defun))

    (if (typescript--re-search-forward
         "\\_<function\\_>" nil t)
        (goto-char (typescript--function-prologue-beginning))
      (goto-char (point-max))))

  (while (> arg 0)
    (decf arg)
    ;; If we're just past the end of a function, the user probably wants
    ;; to go to the beginning of *that* function
    (when (eq (char-before) ?})
      (backward-char))

    (let ((prologue-begin (typescript--function-prologue-beginning)))
      (cond ((and prologue-begin (< prologue-begin (point)))
             (goto-char prologue-begin))

            (typescript-flat-functions
             (typescript--beginning-of-defun-flat))
            (t
             (typescript--beginning-of-defun-nested))))))

(defun typescript--flush-caches (&optional beg ignored)
  "Flush the `typescript-mode' syntax cache after position BEG.
BEG defaults to `point-min', meaning to flush the entire cache."
  (interactive)
  (setq beg (or beg (save-restriction (widen) (point-min))))
  (setq typescript--cache-end (min typescript--cache-end beg)))

(defmacro typescript--debug (&rest arguments)
  ;; `(message ,@arguments)
  )

(defun typescript--ensure-cache--pop-if-ended (open-items paren-depth)
  (let ((top-item (car open-items)))
    (when (<= paren-depth (typescript--pitem-paren-depth top-item))
      (assert (not (get-text-property (1- (point)) 'typescript-pend)))
      (put-text-property (1- (point)) (point) 'typescript--pend top-item)
      (setf (typescript--pitem-b-end top-item) (point))
      (setq open-items
            ;; open-items must contain at least two items for this to
            ;; work, but because we push a dummy item to start with,
            ;; that assumption holds.
            (cons (typescript--pitem-add-child (second open-items) top-item)
                  (cddr open-items)))))
  open-items)

(defmacro typescript--ensure-cache--update-parse ()
  "Helper function for `typescript--ensure-cache'.
Update parsing information up to point, referring to parse,
prev-parse-point, goal-point, and open-items bound lexically in
the body of `typescript--ensure-cache'."
  `(progn
     (setq goal-point (point))
     (goto-char prev-parse-point)
     (while (progn
              (setq open-items (typescript--ensure-cache--pop-if-ended
                                open-items (car parse)))
              ;; Make sure parse-partial-sexp doesn't stop because we *entered*
              ;; the given depth -- i.e., make sure we're deeper than the target
              ;; depth.
              (assert (> (nth 0 parse)
                         (typescript--pitem-paren-depth (car open-items))))
              (setq parse (parse-partial-sexp
                           prev-parse-point goal-point
                           (typescript--pitem-paren-depth (car open-items))
                           nil parse))

;;              (let ((overlay (make-overlay prev-parse-point (point))))
;;                (overlay-put overlay 'face '(:background "red"))
;;                (unwind-protect
;;                     (progn
;;                       (typescript--debug "parsed: %S" parse)
;;                       (sit-for 1))
;;                  (delete-overlay overlay)))

              (setq prev-parse-point (point))
              (< (point) goal-point)))

     (setq open-items (typescript--ensure-cache--pop-if-ended
                       open-items (car parse)))))

(defun typescript--show-cache-at-point ()
  (interactive)
  (require 'pp)
  (let ((prop (get-text-property (point) 'typescript--pstate)))
    (with-output-to-temp-buffer "*Help*"
      (pp prop))))

(defun typescript--split-name (string)
  "Split a typescript name into its dot-separated parts.
This also removes any prototype parts from the split name
\(unless the name is just \"prototype\" to start with)."
  (let ((name (save-match-data
                (split-string string "\\." t))))
    (unless (and (= (length name) 1)
                 (equal (car name) "prototype"))

      (setq name (remove "prototype" name)))))

(defvar typescript--guess-function-name-start nil)

(defun typescript--guess-function-name (position)
  "Guess the name of the typescript function at POSITION.
POSITION should be just after the end of the word \"function\".
Return the name of the function, or nil if the name could not be
guessed.

This function clobbers match data.  If we find the preamble
begins earlier than expected while guessing the function name,
set `typescript--guess-function-name-start' to that position; otherwise,
set that variable to nil."
  (setq typescript--guess-function-name-start nil)
  (save-excursion
    (goto-char position)
    (forward-line 0)
    (cond
     ((looking-at typescript--function-heading-3-re)
      (and (eq (match-end 0) position)
           (setq typescript--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1)))

     ((looking-at typescript--function-heading-2-re)
      (and (eq (match-end 0) position)
           (setq typescript--guess-function-name-start (match-beginning 1))
           (match-string-no-properties 1))))))

(defun typescript--clear-stale-cache ()
  ;; Clear any endings that occur after point
  (let (end-prop)
    (save-excursion
      (while (setq end-prop (typescript--forward-text-property
                             'typescript--pend))
        (setf (typescript--pitem-b-end end-prop) nil))))

  ;; Remove any cache properties after this point
  (remove-text-properties (point) (point-max)
                          '(typescript--pstate t typescript--pend t)))

(defun typescript--ensure-cache (&optional limit)
  "Ensures brace cache is valid up to the character before LIMIT.
LIMIT defaults to point."
  (setq limit (or limit (point)))
  (when (< typescript--cache-end limit)

    (c-save-buffer-state
        (open-items
         orig-match-start
         orig-match-end
         orig-depth
         parse
         prev-parse-point
         name
         case-fold-search
         filtered-class-styles
         new-item
         goal-point
         end-prop)

      ;; Figure out which class styles we need to look for
      (setq filtered-class-styles
            (loop for style in typescript--class-styles
                  if (memq (plist-get style :framework)
                           typescript-enabled-frameworks)
                  collect style))

      (save-excursion
        (save-restriction
          (widen)

          ;; Find last known good position
          (goto-char typescript--cache-end)
          (unless (bobp)
            (setq open-items (get-text-property
                              (1- (point)) 'typescript--pstate))

            (unless open-items
              (goto-char (previous-single-property-change
                          (point) 'typescript--pstate nil (point-min)))

              (unless (bobp)
                (setq open-items (get-text-property (1- (point))
                                                    'typescript--pstate))
                (assert open-items))))

          (unless open-items
            ;; Make a placeholder for the top-level definition
            (setq open-items (list typescript--initial-pitem)))

          (setq parse (syntax-ppss))
          (setq prev-parse-point (point))

          (typescript--clear-stale-cache)

          (narrow-to-region (point-min) limit)

          (loop while (re-search-forward typescript--quick-match-re-func nil t)
                for orig-match-start = (goto-char (match-beginning 0))
                for orig-match-end = (match-end 0)
                do (typescript--ensure-cache--update-parse)
                for orig-depth = (nth 0 parse)

                ;; Each of these conditions should return non-nil if
                ;; we should add a new item and leave point at the end
                ;; of the new item's header (h-end in the
                ;; typescript--pitem diagram). This point is the one
                ;; after the last character we need to unambiguously
                ;; detect this construct. If one of these evaluates to
                ;; nil, the location of the point is ignored.
                if (cond
                    ;; In comment or string
                    ((nth 8 parse) nil)

                    ;; Regular function declaration
                    ((and (looking-at "\\_<function\\_>")
                          (setq name (typescript--forward-function-decl)))

                     (when (eq name t)
                       (setq name (typescript--guess-function-name orig-match-end))
                       (if name
                           (when typescript--guess-function-name-start
                             (setq orig-match-start
                                   typescript--guess-function-name-start))

                         (setq name t)))

                     (assert (eq (char-after) ?{))
                     (forward-char)
                     (make-typescript--pitem
                      :paren-depth orig-depth
                      :h-begin orig-match-start
                      :type 'function
                      :name (if (eq name t)
                                name
                              (typescript--split-name name))))

                    ;; Macro
                    ((looking-at typescript--macro-decl-re)

                     ;; Macros often contain unbalanced parentheses.
                     ;; Make sure that h-end is at the textual end of
                     ;; the macro no matter what the parenthesis say.
                     (c-end-of-macro)
                     (typescript--ensure-cache--update-parse)

                     (make-typescript--pitem
                      :paren-depth (nth 0 parse)
                      :h-begin orig-match-start
                      :type 'macro
                      :name (list (match-string-no-properties 1))))

                    ;; "Prototype function" declaration
                    ((looking-at typescript--plain-method-re)
                     (goto-char (match-beginning 3))
                     (when (save-match-data
                             (typescript--forward-function-decl))
                       (forward-char)
                       (make-typescript--pitem
                        :paren-depth orig-depth
                        :h-begin orig-match-start
                        :type 'function
                        :name (nconc (typescript--split-name
                                      (match-string-no-properties 1))
                                     (list (match-string-no-properties 2))))))

                    ;; Class definition
                    ((loop with syntactic-context =
                           (typescript--syntactic-context-from-pstate open-items)
                           for class-style in filtered-class-styles
                           if (and (memq syntactic-context
                                         (plist-get class-style :contexts))
                                   (looking-at (plist-get class-style
                                                          :class-decl)))
                           do (goto-char (match-end 0))
                           and return
                           (make-typescript--pitem
                            :paren-depth orig-depth
                            :h-begin orig-match-start
                            :type class-style
                            :name (typescript--split-name
                                   (match-string-no-properties 1))))))

                do (typescript--ensure-cache--update-parse)
                and do (push it open-items)
                and do (put-text-property
                        (1- (point)) (point) 'typescript--pstate open-items)
                else do (goto-char orig-match-end))

          (goto-char limit)
          (typescript--ensure-cache--update-parse)
          (setq typescript--cache-end limit)
          (setq typescript--last-parse-pos limit)
          (setq typescript--state-at-last-parse-pos open-items)
          )))))

(defun typescript--end-of-defun-flat ()
  "Helper function for `typescript-end-of-defun'."
  (loop while (typescript--re-search-forward "}" nil t)
        do (typescript--ensure-cache)
        if (get-text-property (1- (point)) 'typescript--pend)
        if (eq 'function (typescript--pitem-type it))
        return t
        finally do (goto-char (point-max))))

(defun typescript--end-of-defun-nested ()
  "Helper function for `typescript-end-of-defun'."
  (message "test")
  (let* (pitem
         (this-end (save-excursion
                     (and (setq pitem (typescript--beginning-of-defun-nested))
                          (typescript--pitem-goto-h-end pitem)
                          (progn (backward-char)
                                 (forward-list)
                                 (point)))))
         found)

    (if (and this-end (< (point) this-end))
        ;; We're already inside a function; just go to its end.
        (goto-char this-end)

      ;; Otherwise, go to the end of the next function...
      (while (and (typescript--re-search-forward "\\_<function\\_>" nil t)
                  (not (setq found (progn
                                     (goto-char (match-beginning 0))
                                     (typescript--forward-function-decl))))))

      (if found (forward-list)
        ;; ... or eob.
        (goto-char (point-max))))))

(defun typescript-end-of-defun (&optional arg)
  "Value of `end-of-defun-function' for `typescript-mode'."
  (setq arg (or arg 1))
  (while (and (not (bobp)) (< arg 0))
    (incf arg)
    (typescript-beginning-of-defun)
    (typescript-beginning-of-defun)
    (unless (bobp)
      (typescript-end-of-defun)))

  (while (> arg 0)
    (decf arg)
    ;; look for function backward. if we're inside it, go to that
    ;; function's end. otherwise, search for the next function's end and
    ;; go there
    (if typescript-flat-functions
        (typescript--end-of-defun-flat)

      ;; if we're doing nested functions, see whether we're in the
      ;; prologue. If we are, go to the end of the function; otherwise,
      ;; call typescript--end-of-defun-nested to do the real work
      (let ((prologue-begin (typescript--function-prologue-beginning)))
        (cond ((and prologue-begin (<= prologue-begin (point)))
               (goto-char prologue-begin)
               (re-search-forward "\\_<function")
               (goto-char (match-beginning 0))
               (typescript--forward-function-decl)
               (forward-list))

              (t (typescript--end-of-defun-nested)))))))

(defun typescript--beginning-of-macro (&optional lim)
  (let ((here (point)))
    (save-restriction
      (if lim (narrow-to-region lim (point-max)))
      (beginning-of-line)
      (while (eq (char-before (1- (point))) ?\\)
        (forward-line -1))
      (back-to-indentation)
      (if (and (<= (point) here)
               (looking-at typescript--opt-cpp-start))
          t
        (goto-char here)
        nil))))

(defun typescript--backward-syntactic-ws (&optional lim)
  "Simple implementation of `c-backward-syntactic-ws' for `typescript-mode'."
  (save-restriction
    (when lim (narrow-to-region lim (point-max)))

    (let ((in-macro (save-excursion (typescript--beginning-of-macro)))
          (pos (point)))

      (while (progn (unless in-macro (typescript--beginning-of-macro))
                    (forward-comment most-negative-fixnum)
                    (/= (point)
                        (prog1
                            pos
                          (setq pos (point)))))))))

(defun typescript--forward-syntactic-ws (&optional lim)
  "Simple implementation of `c-forward-syntactic-ws' for `typescript-mode'."
  (save-restriction
    (when lim (narrow-to-region (point-min) lim))
    (let ((pos (point)))
      (while (progn
               (forward-comment most-positive-fixnum)
               (when (eq (char-after) ?#)
                 (c-end-of-macro))
               (/= (point)
                   (prog1
                       pos
                     (setq pos (point)))))))))

;; Like (up-list -1), but only considers lists that end nearby"
(defun typescript--up-nearby-list ()
  (save-restriction
    ;; Look at a very small region so our compuation time doesn't
    ;; explode in pathological cases.
    (narrow-to-region (max (point-min) (- (point) 500)) (point))
    (up-list -1)))

(defun typescript--inside-param-list-p ()
  "Return non-nil iff point is in a function parameter list."
  (ignore-errors
    (save-excursion
      (typescript--up-nearby-list)
      (and (looking-at "(")
           (progn (forward-symbol -1)
                  (or (looking-at "function")
                      (progn (forward-symbol -1)
                             (looking-at "function"))))))))

(defun typescript--inside-dojo-class-list-p ()
  "Return non-nil iff point is in a Dojo multiple-inheritance class block."
  (ignore-errors
    (save-excursion
      (typescript--up-nearby-list)
      (let ((list-begin (point)))
        (forward-line 0)
        (and (looking-at typescript--dojo-class-decl-re)
             (goto-char (match-end 0))
             (looking-at "\"\\s-*,\\s-*\\[")
             (eq (match-end 0) (1+ list-begin)))))))

(defun typescript--syntax-begin-function ()
  (when (< typescript--cache-end (point))
    (goto-char (max (point-min) typescript--cache-end)))

  (let ((pitem))
    (while (and (setq pitem (car (typescript--backward-pstate)))
                (not (eq 0 (typescript--pitem-paren-depth pitem)))))

    (when pitem
      (goto-char (typescript--pitem-h-begin pitem )))))

;;; Font Lock
(defun typescript--make-framework-matcher (framework &rest regexps)
  "Helper function for building `typescript--font-lock-keywords'.
Create a byte-compiled function for matching a concatenation of
REGEXPS, but only if FRAMEWORK is in `typescript-enabled-frameworks'."
  (setq regexps (apply #'concat regexps))
  (byte-compile
   `(lambda (limit)
      (when (memq (quote ,framework) typescript-enabled-frameworks)
        (re-search-forward ,regexps limit t)))))

(defvar typescript--tmp-location nil)
(make-variable-buffer-local 'typescript--tmp-location)

(defun typescript--forward-destructuring-spec (&optional func)
  "Move forward over a typescript destructuring spec.
If FUNC is supplied, call it with no arguments before every
variable name in the spec.  Return true iff this was actually a
spec.  FUNC must preserve the match data."
  (case (char-after)
    (?\[
     (forward-char)
     (while
         (progn
           (forward-comment most-positive-fixnum)
           (cond ((memq (char-after) '(?\[ ?\{))
                  (typescript--forward-destructuring-spec func))

                 ((eq (char-after) ?,)
                  (forward-char)
                  t)

                 ((looking-at typescript--name-re)
                  (and func (funcall func))
                  (goto-char (match-end 0))
                  t))))
     (when (eq (char-after) ?\])
       (forward-char)
       t))

    (?\{
     (forward-char)
     (forward-comment most-positive-fixnum)
     (while
         (when (looking-at typescript--objfield-re)
           (goto-char (match-end 0))
           (forward-comment most-positive-fixnum)
           (and (cond ((memq (char-after) '(?\[ ?\{))
                       (typescript--forward-destructuring-spec func))
                      ((looking-at typescript--name-re)
                       (and func (funcall func))
                       (goto-char (match-end 0))
                       t))
                (progn (forward-comment most-positive-fixnum)
                       (when (eq (char-after) ?\,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t)))))
     (when (eq (char-after) ?\})
       (forward-char)
       t))))

(defun typescript--variable-decl-matcher (limit)
  "Font-lock matcher for variable names in a variable declaration.
This is a cc-mode-style matcher that *always* fails, from the
point of view of font-lock.  It applies highlighting directly with
`font-lock-apply-highlight'."
  (condition-case nil
      (save-restriction
        (narrow-to-region (point-min) limit)

        (let ((first t))
          (forward-comment most-positive-fixnum)
          (while
              (and (or first
                       (when (eq (char-after) ?,)
                         (forward-char)
                         (forward-comment most-positive-fixnum)
                         t))
                   (cond ((looking-at typescript--name-re)
                          (font-lock-apply-highlight
                           '(0 font-lock-variable-name-face))
                          (goto-char (match-end 0)))

                         ((save-excursion
                            (typescript--forward-destructuring-spec))

                          (typescript--forward-destructuring-spec
                           (lambda ()
                             (font-lock-apply-highlight
                              '(0 font-lock-variable-name-face)))))))

            (forward-comment most-positive-fixnum)
            (when (eq (char-after) ?=)
              (forward-char)
              (typescript--forward-expression)
              (forward-comment most-positive-fixnum))

            (setq first nil))))

    ;; Conditions to handle
    (scan-error nil)
    (end-of-buffer nil))

  ;; Matcher always "fails"
  nil)

(defconst typescript--font-lock-keywords-3
  `(
    ;; This goes before keywords-2 so it gets used preferentially
    ;; instead of the keywords in keywords-2. Don't use override
    ;; because that will override syntactic fontification too, which
    ;; will fontify commented-out directives as if they weren't
    ;; commented out.
    ,@cpp-font-lock-keywords ; from font-lock.el

    ,@typescript--font-lock-keywords-2

    ("\\.\\(prototype\\)\\_>"
     (1 font-lock-constant-face))

    ;; Highlights class being declared, in parts
    (typescript--class-decl-matcher
     ,(concat "\\(" typescript--name-re "\\)\\(?:\\.\\|.*$\\)")
     (goto-char (match-beginning 1))
     nil
     (1 font-lock-type-face))

    ;; Highlights parent class, in parts, if available
    (typescript--class-decl-matcher
     ,(concat "\\(" typescript--name-re "\\)\\(?:\\.\\|.*$\\)")
     (if (match-beginning 2)
         (progn
           (setq typescript--tmp-location (match-end 2))
           (goto-char typescript--tmp-location)
           (insert "=")
           (goto-char (match-beginning 2)))
       (setq typescript--tmp-location nil)
       (goto-char (point-at-eol)))
     (when typescript--tmp-location
       (save-excursion
         (goto-char typescript--tmp-location)
         (delete-char 1)))
     (1 font-lock-type-face))

    ;; Highlights parent class
    (typescript--class-decl-matcher
     (2 font-lock-type-face nil t))

    ;; Dojo needs its own matcher to override the string highlighting
    (,(typescript--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" typescript--dotted-name-re "\\)"
       "\\(?:\"\\s-*,\\s-*\\(" typescript--dotted-name-re "\\)\\)?")
     (1 font-lock-type-face t)
     (2 font-lock-type-face nil t))

    ;; Match Dojo base classes. Of course Mojo has to be different
    ;; from everything else under the sun...
    (,(typescript--make-framework-matcher
       'dojo
       "^\\s-*dojo\\.declare\\s-*(\""
       "\\(" typescript--dotted-name-re "\\)\"\\s-*,\\s-*\\[")
     ,(concat "[[,]\\s-*\\(" typescript--dotted-name-re "\\)\\s-*"
              "\\(?:\\].*$\\)?")
     (backward-char)
     (end-of-line)
     (1 font-lock-type-face))

    ;; continued Dojo base-class list
    (,(typescript--make-framework-matcher
       'dojo
       "^\\s-*" typescript--dotted-name-re "\\s-*[],]")
     ,(concat "\\(" typescript--dotted-name-re "\\)"
              "\\s-*\\(?:\\].*$\\)?")
     (if (save-excursion (backward-char)
                         (typescript--inside-dojo-class-list-p))
         (forward-symbol -1)
       (end-of-line))
     (end-of-line)
     (1 font-lock-type-face))

    ;; variable declarations
    ,(list
      (concat "\\_<\\(const\\|var\\|let\\)\\_>\\|" typescript--basic-type-re)
      (list #'typescript--variable-decl-matcher nil nil nil))

    ;; class instantiation
    ,(list
      (concat "\\_<new\\_>\\s-+\\(" typescript--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; instanceof
    ,(list
      (concat "\\_<instanceof\\_>\\s-+\\(" typescript--dotted-name-re "\\)")
      (list 1 'font-lock-type-face))

    ;; formal parameters
    ,(list
      (concat
       "\\_<function\\_>\\(\\s-+" typescript--name-re "\\)?\\s-*(\\s-*"
       typescript--name-start-re)
      (list (concat "\\(" typescript--name-re "\\)\\(\\s-*).*\\)?")
            '(backward-char)
            '(end-of-line)
            '(1 font-lock-variable-name-face)))

    ;; continued formal parameter list
    ,(list
      (concat
       "^\\s-*" typescript--name-re "\\s-*[,)]")
      (list typescript--name-re
            '(if (save-excursion (backward-char)
                                 (typescript--inside-param-list-p))
                 (forward-symbol -1)
               (end-of-line))
            '(end-of-line)
            '(0 font-lock-variable-name-face))))
  "Level three font lock for `typescript-mode'.")

(defun typescript--inside-pitem-p (pitem)
  "Return whether point is inside the given pitem's header or body."
  (typescript--ensure-cache)
  (assert (typescript--pitem-h-begin pitem))
  (assert (typescript--pitem-paren-depth pitem))

  (and (> (point) (typescript--pitem-h-begin pitem))
       (or (null (typescript--pitem-b-end pitem))
           (> (typescript--pitem-b-end pitem) (point)))))

(defun typescript--parse-state-at-point ()
  "Parse the typescript program state at point.
Return a list of `typescript--pitem' instances that apply to point, most
specific first.  In the worst case, the current toplevel instance
will be returned."
  (save-excursion
    (save-restriction
      (widen)
      (typescript--ensure-cache)
      (let* ((bound (if (eobp) (point) (1+ (point))))
             (pstate (or (save-excursion
                           (typescript--backward-pstate))
                         (list typescript--initial-pitem))))

        ;; Loop until we either hit a pitem at BOB or pitem ends after
        ;; point (or at point if we're at eob)
        (loop for pitem = (car pstate)
              until (or (eq (typescript--pitem-type pitem)
                            'toplevel)
                        (typescript--inside-pitem-p pitem))
              do (pop pstate))

        pstate))))

(defun typescript--syntactic-context-from-pstate (pstate)
  "Return the typescript syntactic context corresponding to PSTATE."
  (let ((type (typescript--pitem-type (car pstate))))
    (cond ((memq type '(function macro))
           type)
          ((consp type)
           'class)
          (t 'toplevel))))

(defun typescript-syntactic-context ()
  "Return the typescript syntactic context at point.
When called interatively, also display a message with that
context."
  (interactive)
  (let* ((syntactic-context (typescript--syntactic-context-from-pstate
                             (typescript--parse-state-at-point))))

    (when (called-interactively-p 'interactive)
      (message "Syntactic context: %s" syntactic-context))

    syntactic-context))

(defun typescript--class-decl-matcher (limit)
  "Font lock function used by `typescript-mode'.
This performs fontification according to `typescript--class-styles'."
  (loop initially (typescript--ensure-cache limit)
        while (re-search-forward typescript--quick-match-re limit t)
        for orig-end = (match-end 0)
        do (goto-char (match-beginning 0))
        if (loop for style in typescript--class-styles
                 for decl-re = (plist-get style :class-decl)
                 if (and (memq (plist-get style :framework)
                               typescript-enabled-frameworks)
                         (memq (typescript-syntactic-context)
                               (plist-get style :contexts))
                         decl-re
                         (looking-at decl-re))
                 do (goto-char (match-end 0))
                 and return t)
        return t
        else do (goto-char orig-end)))

(defconst typescript--font-lock-keywords
  '(typescript--font-lock-keywords-3 typescript--font-lock-keywords-1
                                   typescript--font-lock-keywords-2
                                   typescript--font-lock-keywords-3)
  "Font lock keywords for `typescript-mode'.  See `font-lock-keywords'.")

;; XXX: typescript can continue a regexp literal across lines so long
;; as the newline is escaped with \. Account for that in the regexp
;; below.
(defconst typescript--regexp-literal
  "[=(,:]\\(?:\\s-\\|\n\\)*\\(/\\)\\(?:\\\\/\\|[^/*]\\)\\(?:\\\\/\\|[^/]\\)*\\(/\\)"
  "Regexp matching a typescript regular expression literal.
Match groups 1 and 2 are the characters forming the beginning and
end of the literal.")

;; we want to match regular expressions only at the beginning of
;; expressions
(defconst typescript-font-lock-syntactic-keywords
  `((,typescript--regexp-literal (1 "|") (2 "|")))
  "Syntactic font lock keywords matching regexps in typescript.
See `font-lock-keywords'.")

;;; Indentation

(defconst typescript--possibly-braceless-keyword-re
  (typescript--regexp-opt-symbol
   '("catch" "do" "else" "finally" "for" "if" "try" "while" "with"
     "each"))
  "Regexp matching keywords optionally followed by an opening brace.")

(defconst typescript--indent-operator-re
  (concat "[-+*/%<>=&^|?:.]\\([^-+*/]\\|$\\)\\|"
          (typescript--regexp-opt-symbol '("in" "instanceof")))
  "Regexp matching operators that affect indentation of continued expressions.")


(defun typescript--looking-at-operator-p ()
  "Return non-nil if point is on a typescript operator, other than a comma."
  (save-match-data
    (and (looking-at typescript--indent-operator-re)
         (or (not (looking-at ":"))
             (save-excursion
               (and (typescript--re-search-backward "[?:{]\\|\\_<case\\_>" nil t)
                    (looking-at "?")))))))


(defun typescript--continued-expression-p ()
  "Return non-nil if the current line continues an expression."
  (save-excursion
    (back-to-indentation)
    (or (typescript--looking-at-operator-p)
        (and (typescript--re-search-backward "\n" nil t)
	     (progn
	       (skip-chars-backward " \t")
	       (or (bobp) (backward-char))
	       (and (> (point) (point-min))
                    (save-excursion (backward-char) (not (looking-at "[/*]/")))
                    (typescript--looking-at-operator-p)
		    (and (progn (backward-char)
				(not (looking-at "++\\|--\\|/[/*]"))))))))))


(defun typescript--end-of-do-while-loop-p ()
  "Return non-nil if point is on the \"while\" of a do-while statement.
Otherwise, return nil.  A braceless do-while statement spanning
several lines requires that the start of the loop is indented to
the same column as the current line."
  (interactive)
  (save-excursion
    (save-match-data
      (when (looking-at "\\s-*\\_<while\\_>")
	(if (save-excursion
	      (skip-chars-backward "[ \t\n]*}")
	      (looking-at "[ \t\n]*}"))
	    (save-excursion
	      (backward-list) (forward-symbol -1) (looking-at "\\_<do\\_>"))
	  (typescript--re-search-backward "\\_<do\\_>" (point-at-bol) t)
	  (or (looking-at "\\_<do\\_>")
	      (let ((saved-indent (current-indentation)))
		(while (and (typescript--re-search-backward "^\\s-*\\_<" nil t)
			    (/= (current-indentation) saved-indent)))
		(and (looking-at "\\s-*\\_<do\\_>")
		     (not (typescript--re-search-forward
			   "\\_<while\\_>" (point-at-eol) t))
		     (= (current-indentation) saved-indent)))))))))


(defun typescript--ctrl-statement-indentation ()
  "Helper function for `typescript--proper-indentation'.
Return the proper indentation of the current line if it starts
the body of a control statement without braces; otherwise, return
nil."
  (save-excursion
    (back-to-indentation)
    (when (save-excursion
            (and (not (eq (point-at-bol) (point-min)))
                 (not (looking-at "[{]"))
                 (progn
                   (typescript--re-search-backward "[[:graph:]]" nil t)
                   (or (eobp) (forward-char))
                   (when (= (char-before) ?\)) (backward-list))
                   (skip-syntax-backward " ")
                   (skip-syntax-backward "w_")
                   (looking-at typescript--possibly-braceless-keyword-re))
                 (not (typescript--end-of-do-while-loop-p))))
      (save-excursion
        (goto-char (match-beginning 0))
        (+ (current-indentation) typescript-indent-level)))))

(defun typescript--get-c-offset (symbol anchor)
  (let ((c-offsets-alist
         (list (cons 'c typescript-comment-lineup-func))))
    (c-get-syntactic-indentation (list (cons symbol anchor)))))

(defun typescript--proper-indentation (parse-status)
  "Return the proper indentation for the current line."
  (save-excursion
    (back-to-indentation)
    (cond ((nth 4 parse-status)
           (typescript--get-c-offset 'c (nth 8 parse-status)))
          ((nth 8 parse-status) 0) ; inside string
          ((typescript--ctrl-statement-indentation))
          ((eq (char-after) ?#) 0)
          ((save-excursion (typescript--beginning-of-macro)) 4)
          ((nth 1 parse-status)
           (let ((same-indent-p (looking-at
                                 "[]})]\\|\\_<case\\_>\\|\\_<default\\_>"))
                 (continued-expr-p (typescript--continued-expression-p)))
             (goto-char (nth 1 parse-status))
             (if (looking-at "[({[]\\s-*\\(/[/*]\\|$\\)")
                 (progn
                   (skip-syntax-backward " ")
		   (when (eq (char-before) ?\)) (backward-list))
                   (back-to-indentation)
                   (cond (same-indent-p
                          (current-column))
                         (continued-expr-p
                          (+ (current-column) (* 2 typescript-indent-level)
                             typescript-expr-indent-offset))
                         (t
                          (+ (current-column) typescript-indent-level))))
               (unless same-indent-p
                 (forward-char)
                 (skip-chars-forward " \t"))
               (current-column))))

          ((typescript--continued-expression-p)
           (+ typescript-indent-level typescript-expr-indent-offset))
          (t 0))))

(defun typescript-indent-line ()
  "Indent the current line as typescript."
  (interactive)
  (save-restriction
    (widen)
    (let* ((parse-status
            (save-excursion (syntax-ppss (point-at-bol))))
           (offset (- (current-column) (current-indentation))))
      (indent-line-to (typescript--proper-indentation parse-status))
      (when (> offset 0) (forward-char offset)))))

;;; Filling

(defun typescript-c-fill-paragraph (&optional justify)
  "Fill the paragraph with `c-fill-paragraph'."
  (interactive "*P")
  ;; Dynamically replace functions using the lexically scoped cl-letf.
  ;; See below for more details:
  ;; http://endlessparentheses.com/understanding-letf-and-how-it-replaces-flet.html
  (cl-letf (((symbol-function 'c-forward-sws)
             (lambda  (&optional limit)
               (typescript--forward-syntactic-ws limit)))
            ((symbol-function 'c-backward-sws)
             (lambda  (&optional limit)
               (typescript--backward-syntactic-ws limit)))
            ((symbol-function 'c-beginning-of-macro)
             (lambda  (&optional limit)
               (typescript--beginning-of-macro limit))))
    (let ((fill-paragraph-function 'c-fill-paragraph))
      (c-fill-paragraph justify))))

;;; Type database and Imenu

;; We maintain a cache of semantic information, i.e., the classes and
;; functions we've encountered so far. In order to avoid having to
;; re-parse the buffer on every change, we cache the parse state at
;; each interesting point in the buffer. Each parse state is a
;; modified copy of the previous one, or in the case of the first
;; parse state, the empty state.
;;
;; The parse state itself is just a stack of typescript--pitem
;; instances. It starts off containing one element that is never
;; closed, that is initially typescript--initial-pitem.
;;


(defun typescript--pitem-format (pitem)
  (let ((name (typescript--pitem-name pitem))
        (type (typescript--pitem-type pitem)))

    (format "name:%S type:%S"
            name
            (if (atom type)
                type
              (plist-get type :name)))))

(defun typescript--make-merged-item (item child name-parts)
  "Helper function for `typescript--splice-into-items'.
Return a new item that is the result of merging CHILD into
ITEM.  NAME-PARTS is a list of parts of the name of CHILD
that we haven't consumed yet."
  (typescript--debug "typescript--make-merged-item: {%s} into {%s}"
                   (typescript--pitem-format child)
                   (typescript--pitem-format item))

  ;; If the item we're merging into isn't a class, make it into one
  (unless (consp (typescript--pitem-type item))
    (typescript--debug "typescript--make-merged-item: changing dest into class")
    (setq item (make-typescript--pitem
                :children (list item)

                ;; Use the child's class-style if it's available
                :type (if (atom (typescript--pitem-type child))
                          typescript--dummy-class-style
                  (typescript--pitem-type child))

                :name (typescript--pitem-strname item))))

  ;; Now we can merge either a function or a class into a class
  (cons (cond
         ((cdr name-parts)
          (typescript--debug "typescript--make-merged-item: recursing")
          ;; if we have more name-parts to go before we get to the
          ;; bottom of the class hierarchy, call the merger
          ;; recursively
          (typescript--splice-into-items (car item) child
                                       (cdr name-parts)))

         ((atom (typescript--pitem-type child))
          (typescript--debug "typescript--make-merged-item: straight merge")
          ;; Not merging a class, but something else, so just prepend
          ;; it
          (cons child (car item)))

         (t
          ;; Otherwise, merge the new child's items into those
          ;; of the new class
          (typescript--debug "typescript--make-merged-item: merging class contents")
          (append (car child) (car item))))
        (cdr item)))

(defun typescript--pitem-strname (pitem)
  "Last part of the name of PITEM, as a string or symbol."
  (let ((name (typescript--pitem-name pitem)))
    (if (consp name)
        (car (last name))
      name)))

(defun typescript--splice-into-items (items child name-parts)
  "Splice CHILD into the `typescript--pitem' ITEMS at NAME-PARTS.
If a class doesn't exist in the tree, create it.  Return
the new items list.  NAME-PARTS is a list of strings given
the broken-down class name of the item to insert."

  (let ((top-name (car name-parts))
        (item-ptr items)
        new-items last-new-item new-cons item)

    (typescript--debug "typescript--splice-into-items: name-parts: %S items:%S"
             name-parts
             (mapcar #'typescript--pitem-name items))

    (assert (stringp top-name))
    (assert (> (length top-name) 0))

    ;; If top-name isn't found in items, then we build a copy of items
    ;; and throw it away. But that's okay, since most of the time, we
    ;; *will* find an instance.

    (while (and item-ptr
                (cond ((equal (typescript--pitem-strname (car item-ptr)) top-name)
                       ;; Okay, we found an entry with the right name. Splice
                       ;; the merged item into the list...
                       (setq new-cons (cons (typescript--make-merged-item
                                             (car item-ptr) child
                                             name-parts)
                                            (cdr item-ptr)))

                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))

                       ;; ...and terminate the loop
                       nil)

                      (t
                       ;; Otherwise, copy the current cons and move onto the
                       ;; text. This is tricky; we keep track of the tail of
                       ;; the list that begins with new-items in
                       ;; last-new-item.
                       (setq new-cons (cons (car item-ptr) nil))
                       (if last-new-item
                           (setcdr last-new-item new-cons)
                         (setq new-items new-cons))
                       (setq last-new-item new-cons)

                       ;; Go to the next cell in items
                       (setq item-ptr (cdr item-ptr))))))

    (if item-ptr
        ;; Yay! We stopped because we found something, not because
        ;; we ran out of items to search. Just return the new
        ;; list.
        (progn
          (typescript--debug "search succeeded: %S" name-parts)
          new-items)

      ;; We didn't find anything. If the child is a class and we don't
      ;; have any classes to drill down into, just push that class;
      ;; otherwise, make a fake class and carry on.
      (typescript--debug "search failed: %S" name-parts)
      (cons (if (cdr name-parts)
                ;; We have name-parts left to process. Make a fake
                ;; class for this particular part...
                (make-typescript--pitem
                 ;; ...and recursively digest the rest of the name
                 :children (typescript--splice-into-items
                            nil child (cdr name-parts))
                 :type typescript--dummy-class-style
                 :name top-name)

              ;; Otherwise, this is the only name we have, so stick
              ;; the item on the front of the list
              child)
            items))))

(defun typescript--pitem-add-child (pitem child)
  "Copy `typescript--pitem' PITEM, and push CHILD onto its list of children."
  (assert (integerp (typescript--pitem-h-begin child)))
  (assert (if (consp (typescript--pitem-name child))
              (loop for part in (typescript--pitem-name child)
                    always (stringp part))
            t))

  ;; This trick works because we know (based on our defstructs) that
  ;; the child list is always the first element, and so the second
  ;; element and beyond can be shared when we make our "copy".
  (cons

   (let ((name (typescript--pitem-name child))
         (type (typescript--pitem-type child)))

     (cond ((cdr-safe name) ; true if a list of at least two elements
            ;; Use slow path because we need class lookup
            (typescript--splice-into-items (car pitem) child name))

           ((and (consp type)
                 (plist-get type :prototype))

            ;; Use slow path because we need class merging. We know
            ;; name is a list here because down in
            ;; `typescript--ensure-cache', we made sure to only add
            ;; class entries with lists for :name
            (assert (consp name))
            (typescript--splice-into-items (car pitem) child name))

           (t
            ;; Fast path
            (cons child (car pitem)))))

   (cdr pitem)))

(defun typescript--maybe-make-marker (location)
  "Return a marker for LOCATION if `imenu-use-markers' is non-nil."
  (if imenu-use-markers
      (set-marker (make-marker) location)
    location))

(defun typescript--pitems-to-imenu (pitems unknown-ctr)
  "Convert PITEMS, a list of `typescript--pitem' structures, to imenu format."

  (let (imenu-items pitem pitem-type pitem-name subitems)

    (while (setq pitem (pop pitems))
      (setq pitem-type (typescript--pitem-type pitem))
      (setq pitem-name (typescript--pitem-strname pitem))
      (when (eq pitem-name t)
        (setq pitem-name (format "[unknown %s]"
                                 (incf (car unknown-ctr)))))

      (cond
       ((memq pitem-type '(function macro))
        (assert (integerp (typescript--pitem-h-begin pitem)))
        (push (cons pitem-name
                    (typescript--maybe-make-marker
                     (typescript--pitem-h-begin pitem)))
              imenu-items))

       ((consp pitem-type) ; class definition
        (setq subitems (typescript--pitems-to-imenu
                        (typescript--pitem-children pitem)
                        unknown-ctr))
        (cond (subitems
               (push (cons pitem-name subitems)
                     imenu-items))

              ((typescript--pitem-h-begin pitem)
               (assert (integerp (typescript--pitem-h-begin pitem)))
               (setq subitems (list
                               (cons "[empty]"
                                     (typescript--maybe-make-marker
                                      (typescript--pitem-h-begin pitem)))))
               (push (cons pitem-name subitems)
                     imenu-items))))

       (t (error "Unknown item type: %S" pitem-type))))

    imenu-items))

(defun typescript--imenu-create-index ()
  "Return an imenu index for the current buffer."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-max))
      (typescript--ensure-cache)
      (assert (or (= (point-min) (point-max))
                  (eq typescript--last-parse-pos (point))))
      (when typescript--last-parse-pos
        (let ((state typescript--state-at-last-parse-pos)
              (unknown-ctr (cons -1 nil)))

          ;; Make sure everything is closed
          (while (cdr state)
            (setq state
                  (cons (typescript--pitem-add-child (second state) (car state))
                        (cddr state))))

          (assert (= (length state) 1))

          ;; Convert the new-finalized state into what imenu expects
          (typescript--pitems-to-imenu
           (car (typescript--pitem-children state))
           unknown-ctr))))))

;; Silence the compiler.
(defvar which-func-imenu-joiner-function)

(defun typescript--which-func-joiner (parts)
  (mapconcat #'identity parts "."))

(defun typescript--imenu-to-flat (items prefix symbols)
  (loop for item in items
        if (imenu--subalist-p item)
        do (typescript--imenu-to-flat
            (cdr item) (concat prefix (car item) ".")
            symbols)
        else
        do (let* ((name (concat prefix (car item)))
                  (name2 name)
                  (ctr 0))

             (while (gethash name2 symbols)
               (setq name2 (format "%s<%d>" name (incf ctr))))

             (puthash name2 (cdr item) symbols))))

(defun typescript--get-all-known-symbols ()
  "Return a hash table of all typescript symbols.
This searches all existing `typescript-mode' buffers. Each key is the
name of a symbol (possibly disambiguated with <N>, where N > 1),
and each value is a marker giving the location of that symbol."
  (loop with symbols = (make-hash-table :test 'equal)
        with imenu-use-markers = t
        for buffer being the buffers
        for imenu-index = (with-current-buffer buffer
                            (when (eq major-mode 'typescript-mode)
                              (typescript--imenu-create-index)))
        do (typescript--imenu-to-flat imenu-index "" symbols)
        finally return symbols))

(defvar typescript--symbol-history nil
  "History of entered typescript symbols.")

(defun typescript--read-symbol (symbols-table prompt &optional initial-input)
  "Helper function for `typescript-find-symbol'.
Read a symbol from SYMBOLS-TABLE, which is a hash table like the
one from `typescript--get-all-known-symbols', using prompt PROMPT and
initial input INITIAL-INPUT.  Return a cons of (SYMBOL-NAME
. LOCATION), where SYMBOL-NAME is a string and LOCATION is a
marker."
  (unless ido-mode
    (ido-mode t)
    (ido-mode nil))

  (let ((choice (ido-completing-read
                 prompt
                 (loop for key being the hash-keys of symbols-table
                       collect key)
                 nil t initial-input 'typescript--symbol-history)))
    (cons choice (gethash choice symbols-table))))

(defun typescript--guess-symbol-at-point ()
  (let ((bounds (bounds-of-thing-at-point 'symbol)))
    (when bounds
      (save-excursion
        (goto-char (car bounds))
        (when (eq (char-before) ?.)
          (backward-char)
          (setf (car bounds) (point))))
      (buffer-substring (car bounds) (cdr bounds)))))

(defun typescript-find-symbol (&optional arg)
  "Read a typescript symbol and jump to it.
With a prefix argument, restrict symbols to those from the
current buffer.  Pushes a mark onto the tag ring just like
`find-tag'."
  (interactive "P")
  (let (symbols marker)
    (if (not arg)
        (setq symbols (typescript--get-all-known-symbols))
      (setq symbols (make-hash-table :test 'equal))
      (typescript--imenu-to-flat (typescript--imenu-create-index)
                               "" symbols))

    (setq marker (cdr (typescript--read-symbol
                       symbols "Jump to: "
                       (typescript--guess-symbol-at-point))))

    (ring-insert find-tag-marker-ring (point-marker))
    (switch-to-buffer (marker-buffer marker))
    (push-mark)
    (goto-char marker)))

;;; MozRepl integration

(put 'typescript-moz-bad-rpc 'error-conditions '(error timeout))
(put 'typescript-moz-bad-rpc 'error-message "Mozilla RPC Error")

(put 'typescript-typescript-error 'error-conditions '(error typescript-error))
(put 'typescript-typescript-error 'error-message "typescript Error")

(defun typescript--wait-for-matching-output
  (process regexp timeout &optional start)
  "Wait TIMEOUT seconds for PROCESS to output a match for REGEXP.
On timeout, return nil.  On success, return t with match data
set.  If START is non-nil, look for output starting from START.
Otherwise, use the current value of `process-mark'."
  (with-current-buffer (process-buffer process)
    (loop with start-pos = (or start
                               (marker-position (process-mark process)))
          with end-time = (+ (float-time) timeout)
          for time-left = (- end-time (float-time))
          do (goto-char (point-max))
          if (looking-back regexp start-pos) return t
          while (> time-left 0)
          do (accept-process-output process time-left nil t)
          do (goto-char (process-mark process))
          finally do (signal
                      'typescript-moz-bad-rpc
                      (list (format "Timed out waiting for output matching %S" regexp))))))

(defstruct typescript--typescript-handle
  ;; Integer, mirrors the value we see in TYPESCRIPT
  (id nil :read-only t)

  ;; Process to which this thing belongs
  (process nil :read-only t))

(defun typescript--typescript-handle-expired-p (x)
  (not (eq (typescript--typescript-handle-process x)
           (inferior-moz-process))))

(defvar typescript--typescript-references nil
  "Maps Elisp typescript proxy objects to their typescript IDs.")

(defvar typescript--typescript-process nil
  "The most recent MozRepl process object.")

(defvar typescript--typescript-gc-idle-timer nil
  "Idle timer for cleaning up TYPESCRIPT object references.")

(defvar typescript--typescript-last-gcs-done nil)

(defconst typescript--moz-interactor
  (replace-regexp-in-string
   "[ \n]+" " "
"(function(repl) {
  repl.defineInteractor('js', {
    onStart: function onStart(repl) {
      if(!repl._jsObjects) {
        repl._jsObjects = {};
        repl._jsLastID = 0;
        repl._jsGC = this._jsGC;
      }
      this._input = '';
    },

    _jsGC: function _jsGC(ids_in_use) {
      var objects = this._jsObjects;
      var keys = [];
      var num_freed = 0;

      for(var pn in objects) {
        keys.push(Number(pn));
      }

      keys.sort(function(x, y) x - y);
      ids_in_use.sort(function(x, y) x - y);
      var i = 0;
      var j = 0;

      while(i < ids_in_use.length && j < keys.length) {
        var id = ids_in_use[i++];
        while(j < keys.length && keys[j] !== id) {
          var k_id = keys[j++];
          delete objects[k_id];
          ++num_freed;
        }
        ++j;
      }

      while(j < keys.length) {
        var k_id = keys[j++];
        delete objects[k_id];
        ++num_freed;
      }

      return num_freed;
    },

    _mkArray: function _mkArray() {
      var result = [];
      for(var i = 0; i < arguments.length; ++i) {
        result.push(arguments[i]);
      }
      return result;
    },

    _parsePropDescriptor: function _parsePropDescriptor(parts) {
      if(typeof parts === 'string') {
        parts = [ parts ];
      }

      var obj = parts[0];
      var start = 1;

      if(typeof obj === 'string') {
        obj = window;
        start = 0;
      } else if(parts.length < 2) {
        throw new Error('expected at least 2 arguments');
      }

      for(var i = start; i < parts.length - 1; ++i) {
        obj = obj[parts[i]];
      }

      return [obj, parts[parts.length - 1]];
    },

    _getProp: function _getProp(/*...*/) {
      if(arguments.length === 0) {
        throw new Error('no arguments supplied to getprop');
      }

      if(arguments.length === 1 &&
         (typeof arguments[0]) !== 'string')
      {
        return arguments[0];
      }

      var [obj, propname] = this._parsePropDescriptor(arguments);
      return obj[propname];
    },

    _putProp: function _putProp(properties, value) {
      var [obj, propname] = this._parsePropDescriptor(properties);
      obj[propname] = value;
    },

    _delProp: function _delProp(propname) {
      var [obj, propname] = this._parsePropDescriptor(arguments);
      delete obj[propname];
    },

    _typeOf: function _typeOf(thing) {
      return typeof thing;
    },

    _callNew: function(constructor) {
      if(typeof constructor === 'string')
      {
        constructor = window[constructor];
      } else if(constructor.length === 1 &&
                typeof constructor[0] !== 'string')
      {
        constructor = constructor[0];
      } else {
        var [obj,propname] = this._parsePropDescriptor(constructor);
        constructor = obj[propname];
      }

      /* Hacky, but should be robust */
      var s = 'new constructor(';
      for(var i = 1; i < arguments.length; ++i) {
        if(i != 1) {
          s += ',';
        }

        s += 'arguments[' + i + ']';
      }

      s += ')';
      return eval(s);
    },

    _callEval: function(thisobj, js) {
      return eval.call(thisobj, js);
    },

    getPrompt: function getPrompt(repl) {
      return 'EVAL>'
    },

    _lookupObject: function _lookupObject(repl, id) {
      if(typeof id === 'string') {
        switch(id) {
        case 'global':
          return window;
        case 'nil':
          return null;
        case 't':
          return true;
        case 'false':
          return false;
        case 'undefined':
          return undefined;
        case 'repl':
          return repl;
        case 'interactor':
          return this;
        case 'NaN':
          return NaN;
        case 'Infinity':
          return Infinity;
        case '-Infinity':
          return -Infinity;
        default:
          throw new Error('No object with special id:' + id);
        }
      }

      var ret = repl._jsObjects[id];
      if(ret === undefined) {
        throw new Error('No object with id:' + id + '(' + typeof id + ')');
      }
      return ret;
    },

    _findOrAllocateObject: function _findOrAllocateObject(repl, value) {
      if(typeof value !== 'object'  && typeof value !== 'function') {
        throw new Error('_findOrAllocateObject called on non-object('
                        + typeof(value) + '): '
                        + value)
      }

      for(var id in repl._jsObjects) {
        id = Number(id);
        var obj = repl._jsObjects[id];
        if(obj === value) {
          return id;
        }
      }

      var id = ++repl._jsLastID;
      repl._jsObjects[id] = value;
      return id;
    },

    _fixupList: function _fixupList(repl, list) {
      for(var i = 0; i < list.length; ++i) {
        if(list[i] instanceof Array) {
          this._fixupList(repl, list[i]);
        } else if(typeof list[i] === 'object') {
          var obj = list[i];
          if(obj.funcall) {
            var parts = obj.funcall;
            this._fixupList(repl, parts);
            var [thisobj, func] = this._parseFunc(parts[0]);
            list[i] = func.apply(thisobj, parts.slice(1));
          } else if(obj.objid) {
            list[i] = this._lookupObject(repl, obj.objid);
          } else {
            throw new Error('Unknown object type: ' + obj.toSource());
          }
        }
      }
    },

    _parseFunc: function(func) {
      var thisobj = null;

      if(typeof func === 'string') {
        func = window[func];
      } else if(func instanceof Array) {
        if(func.length === 1 && typeof func[0] !== 'string') {
          func = func[0];
        } else {
          [thisobj, func] = this._parsePropDescriptor(func);
          func = thisobj[func];
        }
      }

      return [thisobj,func];
    },

    _encodeReturn: function(value, array_as_mv) {
      var ret;

      if(value === null) {
        ret = ['special', 'null'];
      } else if(value === true) {
        ret = ['special', 'true'];
      } else if(value === false) {
        ret = ['special', 'false'];
      } else if(value === undefined) {
        ret = ['special', 'undefined'];
      } else if(typeof value === 'number') {
        if(isNaN(value)) {
          ret = ['special', 'NaN'];
        } else if(value === Infinity) {
          ret = ['special', 'Infinity'];
        } else if(value === -Infinity) {
          ret = ['special', '-Infinity'];
        } else {
          ret = ['atom', value];
        }
      } else if(typeof value === 'string') {
        ret = ['atom', value];
      } else if(array_as_mv && value instanceof Array) {
        ret = ['array', value.map(this._encodeReturn, this)];
      } else {
        ret = ['objid', this._findOrAllocateObject(repl, value)];
      }

      return ret;
    },

    _handleInputLine: function _handleInputLine(repl, line) {
      var ret;
      var array_as_mv = false;

      try {
        if(line[0] === '*') {
          array_as_mv = true;
          line = line.substring(1);
        }
        var parts = eval(line);
        this._fixupList(repl, parts);
        var [thisobj, func] = this._parseFunc(parts[0]);
        ret = this._encodeReturn(
          func.apply(thisobj, parts.slice(1)),
          array_as_mv);
      } catch(x) {
        ret = ['error', x.toString() ];
      }

      var JSON = Components.classes['@mozilla.org/dom/json;1'].createInstance(Components.interfaces.nsIJSON);
      repl.print(JSON.encode(ret));
      repl._prompt();
    },

    handleInput: function handleInput(repl, chunk) {
      this._input += chunk;
      var match, line;
      while(match = this._input.match(/.*\\n/)) {
        line = match[0];

        if(line === 'EXIT\\n') {
          repl.popInteractor();
          repl._prompt();
          return;
        }

        this._input = this._input.substring(line.length);
        this._handleInputLine(repl, line);
      }
    }
  });
})
")

  "String to set MozRepl up into a simple-minded evaluation mode.")


(defun typescript--typescript-encode-value (x)
  "Marshall the given value for Typescript.
Strings and numbers are JSON-encoded.  Lists (including nil) are
made into typescript array literals and their contents encoded
with `typescript--typescript-encode-value'."
  (cond ((stringp x) (json-encode-string x))
        ((numberp x) (json-encode-number x))
        ((symbolp x) (format "{objid:%S}" (symbol-name x)))
        ((typescript--typescript-handle-p x)

         (when (typescript--typescript-handle-expired-p x)
           (error "Stale TYPESCRIPT handle"))

         (format "{objid:%s}" (typescript--typescript-handle-id x)))

        ((sequencep x)
         (if (eq (car-safe x) 'typescript--funcall)
             (format "{funcall:[%s]}"
                     (mapconcat #'typescript--typescript-encode-value (cdr x) ","))
           (concat
            "[" (mapconcat #'typescript--typescript-encode-value x ",") "]")))
        (t
         (error "Unrecognized item: %S" x))))

(defconst typescript--typescript-prompt-regexp "\\(repl[0-9]*\\)> $")
(defconst typescript--typescript-repl-prompt-regexp "^EVAL>$")
(defvar typescript--typescript-repl-depth 0)

(defun typescript--typescript-wait-for-eval-prompt ()
  (typescript--wait-for-matching-output
   (inferior-moz-process)
   typescript--typescript-repl-prompt-regexp typescript-typescript-timeout

   ;; start matching against the beginning of the line in
   ;; order to catch a prompt that's only partially arrived
   (save-excursion (forward-line 0) (point))))

(defun typescript--typescript-enter-repl ()
  (inferior-moz-process) ; called for side-effect
  (with-current-buffer inferior-moz-buffer
    (goto-char (point-max))

    ;; Do some initialization the first time we see a process
    (unless (eq (inferior-moz-process) typescript--typescript-process)
      (setq typescript--typescript-process (inferior-moz-process))
      (setq typescript--typescript-references (make-hash-table :test 'eq :weakness t))
      (setq typescript--typescript-repl-depth 0)

      ;; Send interactor definition
      (comint-send-string typescript--typescript-process typescript--moz-interactor)
      (comint-send-string typescript--typescript-process
                          (concat "(" moz-repl-name ")\n"))
      (typescript--wait-for-matching-output
       (inferior-moz-process) typescript--typescript-prompt-regexp
       typescript-typescript-timeout))

    ;; Sanity check
    (when (looking-back typescript--typescript-prompt-regexp
                        (save-excursion (forward-line 0) (point)))
      (setq typescript--typescript-repl-depth 0))

    (if (> typescript--typescript-repl-depth 0)
        ;; If typescript--typescript-repl-depth > 0, we *should* be seeing an
        ;; EVAL> prompt. If we don't, give Mozilla a chance to catch
        ;; up with us.
        (typescript--typescript-wait-for-eval-prompt)

      ;; Otherwise, tell Mozilla to enter the interactor mode
      (insert (match-string-no-properties 1)
              ".pushInteractor('typescript')")
      (comint-send-input nil t)
      (typescript--wait-for-matching-output
       (inferior-moz-process) typescript--typescript-repl-prompt-regexp
       typescript-typescript-timeout))

    (incf typescript--typescript-repl-depth)))

(defun typescript--typescript-leave-repl ()
  (assert (> typescript--typescript-repl-depth 0))
  (when (= 0 (decf typescript--typescript-repl-depth))
    (with-current-buffer inferior-moz-buffer
      (goto-char (point-max))
      (typescript--typescript-wait-for-eval-prompt)
      (insert "EXIT")
      (comint-send-input nil t)
      (typescript--wait-for-matching-output
       (inferior-moz-process) typescript--typescript-prompt-regexp
       typescript-typescript-timeout))))

(defsubst typescript--typescript-not (value)
  (memq value '(nil null false undefined)))

(defsubst typescript--typescript-true (value)
  (not (typescript--typescript-not value)))

(eval-and-compile
  (defun typescript--optimize-arglist (arglist)
    "Convert immediate typescript< and typescript! references to deferred ones."
    (loop for item in arglist
          if (eq (car-safe item) 'typescript<)
          collect (append (list 'list ''typescript--funcall
                                '(list 'interactor "_getProp"))
                          (typescript--optimize-arglist (cdr item)))
          else if (eq (car-safe item) 'typescript>)
          collect (append (list 'list ''typescript--funcall
                                '(list 'interactor "_putProp"))

                          (if (atom (cadr item))
                              (list (cadr item))
                            (list
                             (append
                              (list 'list ''typescript--funcall
                                    '(list 'interactor "_mkArray"))
                              (typescript--optimize-arglist (cadr item)))))
                          (typescript--optimize-arglist (cddr item)))
          else if (eq (car-safe item) 'typescript!)
          collect (destructuring-bind (ignored function &rest body) item
                    (append (list 'list ''typescript--funcall
                                  (if (consp function)
                                      (cons 'list
                                            (typescript--optimize-arglist function))
                                    function))
                            (typescript--optimize-arglist body)))
          else
          collect item)))

(defmacro typescript--typescript-get-service (class-name interface-name)
    `(typescript! ("Components" "classes" ,class-name "getService")
        (typescript< "Components" "interfaces" ,interface-name)))

(defmacro typescript--typescript-create-instance (class-name interface-name)
  `(typescript! ("Components" "classes" ,class-name "createInstance")
        (typescript< "Components" "interfaces" ,interface-name)))

(defmacro typescript--typescript-qi (object interface-name)
  `(typescript! (,object "QueryInterface")
        (typescript< "Components" "interfaces" ,interface-name)))

(defmacro with-typescript (&rest forms)
  "Run FORMS with the Mozilla repl set up for typescript commands.
Inside the lexical scope of `with-typescript', `typescript?', `typescript!',
`typescript-new', `typescript-eval', `typescript-list', `typescript<', `typescript>', `typescript-get-service',
`typescript-create-instance', and `typescript-qi' are defined."

  `(progn
     (typescript--typescript-enter-repl)
     (unwind-protect
         (macrolet ((typescript? (&rest body) `(typescript--typescript-true ,@body))
                    (typescript! (function &rest body)
                         `(typescript--typescript-funcall
                           ,(if (consp function)
                                (cons 'list
                                      (typescript--optimize-arglist function))
                              function)
                           ,@(typescript--optimize-arglist body)))

                    (typescript-new (function &rest body)
                            `(typescript--typescript-new
                              ,(if (consp function)
                                   (cons 'list
                                         (typescript--optimize-arglist function))
                                 function)
                              ,@body))

                    (typescript-eval (thisobj typescript)
                            `(typescript--typescript-eval
                              ,@(typescript--optimize-arglist
                                 (list thisobj typescript))))

                    (typescript-list (&rest args)
                             `(typescript--typescript-list
                               ,@(typescript--optimize-arglist args)))

                    (typescript-get-service (&rest args)
                                    `(typescript--typescript-get-service
                                      ,@(typescript--optimize-arglist args)))

                    (typescript-create-instance (&rest args)
                                        `(typescript--typescript-create-instance
                                          ,@(typescript--optimize-arglist args)))

                    (typescript-qi (&rest args)
                           `(typescript--typescript-qi
                             ,@(typescript--optimize-arglist args)))

                    (typescript< (&rest body) `(typescript--typescript-get
                                        ,@(typescript--optimize-arglist body)))
                    (typescript> (props value)
                         `(typescript--typescript-funcall
                           '(interactor "_putProp")
                           ,(if (consp props)
                                (cons 'list
                                      (typescript--optimize-arglist props))
                              props)
                           ,@(typescript--optimize-arglist (list value))
                           ))
                    (typescript-handle? (arg) `(typescript--typescript-handle-p ,arg)))
           ,@forms)
       (typescript--typescript-leave-repl))))

(defvar typescript--typescript-array-as-list nil
  "Whether to listify any Array returned by a Mozilla function.
If nil, the whole Array is treated as a TYPESCRIPT symbol.")

(defun typescript--typescript-decode-retval (result)
  (ecase (intern (first result))
         (atom (second result))
         (special (intern (second result)))
         (array
          (mapcar #'typescript--typescript-decode-retval (second result)))
         (objid
          (or (gethash (second result)
                       typescript--typescript-references)
              (puthash (second result)
                       (make-typescript--typescript-handle
                        :id (second result)
                        :process (inferior-moz-process))
                       typescript--typescript-references)))

         (error (signal 'typescript-typescript-error (list (second result))))))

(defun typescript--typescript-funcall (function &rest arguments)
  "Call the Mozilla function FUNCTION with arguments ARGUMENTS.
If function is a string, look it up as a property on the global
object and use the global object for `this'.
If FUNCTION is a list with one element, use that element as the
function with the global object for `this', except that if that
single element is a string, look it up on the global object.
If FUNCTION is a list with more than one argument, use the list
up to the last value as a property descriptor and the last
argument as a function."

  (with-typescript
   (let ((argstr (typescript--typescript-encode-value
                  (cons function arguments))))

     (with-current-buffer inferior-moz-buffer
       ;; Actual funcall
       (when typescript--typescript-array-as-list
         (insert "*"))
       (insert argstr)
       (comint-send-input nil t)
       (typescript--wait-for-matching-output
        (inferior-moz-process) "EVAL>"
        typescript-typescript-timeout)
       (goto-char comint-last-input-end)

       ;; Read the result
       (let* ((json-array-type 'list)
              (result (prog1 (json-read)
                        (goto-char (point-max)))))
         (typescript--typescript-decode-retval result))))))

(defun typescript--typescript-new (constructor &rest arguments)
  "Call CONSTRUCTOR as a constructor, with arguments ARGUMENTS.
CONSTRUCTOR is a TYPESCRIPT handle, a string, or a list of these things."
  (apply #'typescript--typescript-funcall
         '(interactor "_callNew")
         constructor arguments))

(defun typescript--typescript-eval (thisobj typescript)
  (typescript--typescript-funcall '(interactor "_callEval") thisobj typescript))

(defun typescript--typescript-list (&rest arguments)
  "Return a Lisp array resulting from evaluating each of ARGUMENTS."
  (let ((typescript--typescript-array-as-list t))
    (apply #'typescript--typescript-funcall '(interactor "_mkArray")
           arguments)))

(defun typescript--typescript-get (&rest props)
  (apply #'typescript--typescript-funcall '(interactor "_getProp") props))

(defun typescript--typescript-put (props value)
  (typescript--typescript-funcall '(interactor "_putProp") props value))

(defun typescript-gc (&optional force)
  "Tell the repl about any objects we don't reference anymore.
With argument, run even if no intervening GC has happened."
  (interactive)

  (when force
    (setq typescript--typescript-last-gcs-done nil))

  (let ((this-gcs-done gcs-done) keys num)
    (when (and typescript--typescript-references
               (boundp 'inferior-moz-buffer)
               (buffer-live-p inferior-moz-buffer)

               ;; Don't bother running unless we've had an intervening
               ;; garbage collection; without a gc, nothing is deleted
               ;; from the weak hash table, so it's pointless telling
               ;; MozRepl about that references we still hold
               (not (eq typescript--typescript-last-gcs-done this-gcs-done))

               ;; Are we looking at a normal prompt? Make sure not to
               ;; interrupt the user if he's doing something
               (with-current-buffer inferior-moz-buffer
                 (save-excursion
                   (goto-char (point-max))
                   (looking-back typescript--typescript-prompt-regexp
                                 (save-excursion (forward-line 0) (point))))))

      (setq keys (loop for x being the hash-keys
                       of typescript--typescript-references
                       collect x))
      (setq num (typescript--typescript-funcall '(repl "_typescriptGC") (or keys [])))

      (setq typescript--typescript-last-gcs-done this-gcs-done)
      (when (called-interactively-p 'interactive)
        (message "Cleaned %s entries" num))

      num)))

(run-with-idle-timer 30 t #'typescript-gc)

(defun typescript-eval (typescript)
  "Evaluate the typescript and return JSON-decoded result."
  (interactive "Mtypescript to evaluate: ")
  (with-typescript
   (let* ((content-window (typescript--typescript-content-window
                           (typescript--get-typescript-context)))
          (result (typescript-eval content-window typescript)))
     (when (called-interactively-p 'interactive)
       (message "%s" (typescript! "String" result)))
     result)))

(defun typescript--get-tabs ()
  "Enumerate all typescript contexts available.
Each context is a list:
   (TITLE URL BROWSER TAB TABBROWSER) for content documents
   (TITLE URL WINDOW) for windows

All tabs of a given window are grouped together.  The most recent
window is first.  Within each window, the tabs are returned
left-to-right."
  (with-typescript
   (let (windows)

     (loop with window-mediator = (typescript! ("Components" "classes"
                                        "@mozilla.org/appshell/window-mediator;1"
                                        "getService")
                                       (typescript< "Components" "interfaces"
                                            "nsIWindowMediator"))
           with enumerator = (typescript! (window-mediator "getEnumerator") nil)

           while (typescript? (typescript! (enumerator "hasMoreElements")))
           for window = (typescript! (enumerator "getNext"))
           for window-info = (typescript-list window
                                      (typescript< window "document" "title")
                                      (typescript! (window "location" "toString"))
                                      (typescript< window "closed")
                                      (typescript< window "windowState"))

           unless (or (typescript? (fourth window-info))
                      (eq (fifth window-info) 2))
           do (push window-info windows))

     (loop for window-info in windows
           for window = (first window-info)
           collect (list (second window-info)
                         (third window-info)
                         window)

           for gbrowser = (typescript< window "gBrowser")
           if (typescript-handle? gbrowser)
           nconc (loop
                  for x below (typescript< gbrowser "browsers" "length")
                  collect (typescript-list (typescript< gbrowser
                                        "browsers"
                                        x
                                        "contentDocument"
                                        "title")

                                   (typescript! (gbrowser
                                         "browsers"
                                         x
                                         "contentWindow"
                                         "location"
                                         "toString"))
                                   (typescript< gbrowser
                                        "browsers"
                                        x)

                                   (typescript! (gbrowser
                                         "tabContainer"
                                         "childNodes"
                                         "item")
                                        x)

                                   gbrowser))))))

(defvar typescript-read-tab-history nil)

(defun typescript--read-tab (prompt)
  "Read a Mozilla tab with prompt PROMPT.
Return a cons of (TYPE . OBJECT).  TYPE is either 'window or
'tab, and OBJECT is a typescript handle to a ChromeWindow or a
browser, respectively."

  ;; Prime IDO
  (unless ido-mode
    (ido-mode t)
    (ido-mode nil))

  (with-typescript
   (lexical-let ((tabs (typescript--get-tabs)) selected-tab-cname
                 selected-tab prev-hitab)

     ;; Disambiguate names
     (setq tabs (loop with tab-names = (make-hash-table :test 'equal)
                      for tab in tabs
                      for cname = (format "%s (%s)" (second tab) (first tab))
                      for num = (incf (gethash cname tab-names -1))
                      if (> num 0)
                      do (setq cname (format "%s <%d>" cname num))
                      collect (cons cname tab)))

     (labels ((find-tab-by-cname
               (cname)
               (loop for tab in tabs
                     if (equal (car tab) cname)
                     return (cdr tab)))

              (mogrify-highlighting
               (hitab unhitab)

               ;; Hack to reduce the number of
               ;; round-trips to mozilla
               (let (cmds)
                 (cond
                  ;; Highlighting tab
                  ((fourth hitab)
                   (push '(typescript! ((fourth hitab) "setAttribute")
                                       "style"
                                       "color: red; font-weight: bold")
                         cmds)

                   ;; Highlight window proper
                   (push '(typescript! ((third hitab)
                                        "setAttribute")
                                       "style"
                                       "border: 8px solid red")
                         cmds)

                   ;; Select tab, when appropriate
                   (when typescript-typescript-switch-tabs
                     (push
                      '(typescript> ((fifth hitab) "selectedTab") (fourth hitab))
                      cmds)))

                  ;; Hilighting whole window
                  ((third hitab)
                   (push '(typescript! ((third hitab) "document"
                                        "documentElement" "setAttribute")
                                       "style"
                                       (concat "-moz-appearance: none;"
                                               "border: 8px solid red;"))
                         cmds)))

                 (cond
                  ;; Unhighlighting tab
                  ((fourth unhitab)
                   (push '(typescript! ((fourth unhitab) "setAttribute") "style" "")
                         cmds)
                   (push '(typescript! ((third unhitab) "setAttribute") "style" "")
                         cmds))

                  ;; Unhighlighting window
                  ((third unhitab)
                   (push '(typescript! ((third unhitab) "document"
                                        "documentElement" "setAttribute")
                                       "style" "")
                         cmds)))

                 (eval (list 'with-typescript
                             (cons 'typescript-list (nreverse cmds))))))

              (command-hook
               ()
               (let* ((tab (find-tab-by-cname (car ido-matches))))
                 (mogrify-highlighting tab prev-hitab)
                 (setq prev-hitab tab)))

              (setup-hook
               ()
               ;; Fiddle with the match list a bit: if our first match
               ;; is a tabbrowser window, rotate the match list until
               ;; the active tab comes up
               (let ((matched-tab (find-tab-by-cname (car ido-matches))))
                 (when (and matched-tab
                            (null (fourth matched-tab))
                            (equal "navigator:browser"
                                   (typescript! ((third matched-tab)
                                                 "document"
                                                 "documentElement"
                                                 "getAttribute")
                                                "windowtype")))

                   (loop with tab-to-match = (typescript< (third matched-tab)
                                                          "gBrowser"
                                                          "selectedTab")

                         with index = 0
                         for match in ido-matches
                         for candidate-tab = (find-tab-by-cname match)
                         if (eq (fourth candidate-tab) tab-to-match)
                         do (setq ido-cur-list (ido-chop ido-cur-list match))
                         and return t)))

               (add-hook 'post-command-hook #'command-hook t t)))


       (unwind-protect
           (setq selected-tab-cname
                 (let ((ido-minibuffer-setup-hook
                        (cons #'setup-hook ido-minibuffer-setup-hook)))
                   (ido-completing-read
                    prompt
                    (mapcar #'car tabs)
                    nil t nil
                    'typescript-read-tab-history)))

         (when prev-hitab
           (mogrify-highlighting nil prev-hitab)
           (setq prev-hitab nil)))

       (add-to-history 'typescript-read-tab-history selected-tab-cname)

       (setq selected-tab (loop for tab in tabs
                                if (equal (car tab) selected-tab-cname)
                                return (cdr tab)))

       (if (fourth selected-tab)
           (cons 'browser (third selected-tab))
         (cons 'window (third selected-tab)))))))

(defun typescript--guess-eval-defun-info (pstate)
  "Helper function for `typescript-eval-defun'.
Return a list (NAME . CLASSPARTS), where CLASSPARTS is a list of
strings making up the class name and NAME is the name of the
function part."
  (cond ((and (= (length pstate) 3)
              (eq (typescript--pitem-type (first pstate)) 'function)
              (= (length (typescript--pitem-name (first pstate))) 1)
              (consp (typescript--pitem-type (second pstate))))

         (append (typescript--pitem-name (second pstate))
                 (list (first (typescript--pitem-name (first pstate))))))

        ((and (= (length pstate) 2)
              (eq (typescript--pitem-type (first pstate)) 'function))

         (append
          (butlast (typescript--pitem-name (first pstate)))
          (list (car (last (typescript--pitem-name (first pstate)))))))

        (t (error "Function not a toplevel defun or class member"))))

(defvar typescript--typescript-context nil
  "The current typescript context.
This is a cons like the one returned from `typescript--read-tab'.
Change with `typescript-set-typescript-context'.")

(defconst typescript--typescript-inserter
  "(function(func_info,func) {
    func_info.unshift('window');
    var obj = window;
    for(var i = 1; i < func_info.length - 1; ++i) {
      var next = obj[func_info[i]];
      if(typeof next !== 'object' && typeof next !== 'function') {
        next = obj.prototype && obj.prototype[func_info[i]];
        if(typeof next !== 'object' && typeof next !== 'function') {
          alert('Could not find ' + func_info.slice(0, i+1).join('.') +
                ' or ' + func_info.slice(0, i+1).join('.') + '.prototype');
          return;
        }

        func_info.splice(i+1, 0, 'prototype');
        ++i;
      }
    }

    obj[func_info[i]] = func;
    alert('Successfully updated '+func_info.join('.'));
  })")

(defun typescript-set-typescript-context (context)
  "Set the typescript context to CONTEXT.
When called interactively, prompt for CONTEXT."
  (interactive (list (typescript--read-tab "typescript Context: ")))
  (setq typescript--typescript-context context))

(defun typescript--get-typescript-context ()
  "Return a valid typescript context.
If one hasn't been set, or if it's stale, prompt for a new one."
  (with-typescript
   (when (or (null typescript--typescript-context)
             (typescript--typescript-handle-expired-p (cdr typescript--typescript-context))
             (ecase (car typescript--typescript-context)
               (window (typescript? (typescript< (cdr typescript--typescript-context) "closed")))
               (browser (not (typescript? (typescript< (cdr typescript--typescript-context)
                                       "contentDocument"))))))
     (setq typescript--typescript-context (typescript--read-tab "typescript Context: ")))
   typescript--typescript-context))

(defun typescript--typescript-content-window (context)
  (with-typescript
   (ecase (car context)
     (window (cdr context))
     (browser (typescript< (cdr context)
                   "contentWindow" "wrappedTYPESCRIPTObject")))))

(defun typescript--make-nsilocalfile (path)
  (with-typescript
   (let ((file (typescript-create-instance "@mozilla.org/file/local;1"
                                   "nsILocalFile")))
     (typescript! (file "initWithPath") path)
     file)))

(defun typescript--typescript-add-resource-alias (alias path)
  (with-typescript
   (let* ((io-service (typescript-get-service "@mozilla.org/network/io-service;1"
                                                "nsIIOService"))
          (res-prot (typescript! (io-service "getProtocolHandler") "resource"))
          (res-prot (typescript-qi res-prot "nsIResProtocolHandler"))
          (path-file (typescript--make-nsilocalfile path))
          (path-uri (typescript! (io-service "newFileURI") path-file)))
     (typescript! (res-prot "setSubstitution") alias path-uri))))

(defun* typescript-eval-defun ()
  "Update a Mozilla tab using the typescript defun at point."
  (interactive)

  ;; This function works by generating a temporary file that contains
  ;; the function we'd like to insert. We then use the elisp-typescript bridge
  ;; to command mozilla to load this file by inserting a script tag
  ;; into the document we set. This way, debuggers and such will have
  ;; a way to find the source of the just-inserted function.
  ;;
  ;; We delete the temporary file if there's an error, but otherwise
  ;; we add an unload event listener on the Mozilla side to delete the
  ;; file.

  (save-excursion
    (let (begin end pstate defun-info temp-name defun-body)
      (typescript-end-of-defun)
      (setq end (point))
      (typescript--ensure-cache)
      (typescript-beginning-of-defun)
      (re-search-forward "\\_<function\\_>")
      (setq begin (match-beginning 0))
      (setq pstate (typescript--forward-pstate))

      (when (or (null pstate)
                (> (point) end))
        (error "Could not locate function definition"))

      (setq defun-info (typescript--guess-eval-defun-info pstate))

      (let ((overlay (make-overlay begin end)))
        (overlay-put overlay 'face 'highlight)
        (unwind-protect
            (unless (y-or-n-p (format "Send %s to Mozilla? "
                                      (mapconcat #'identity defun-info ".")))
              (message "") ; question message lingers until next command
              (return-from typescript-eval-defun))
          (delete-overlay overlay)))

      (setq defun-body (buffer-substring-no-properties begin end))

      (make-directory typescript-typescript-tmpdir t)

      ;; (Re)register a Mozilla resource URL to point to the
      ;; temporary directory
      (typescript--typescript-add-resource-alias "typescript" typescript-typescript-tmpdir)

      (setq temp-name (make-temp-file (concat typescript-typescript-tmpdir
                                             "/typescript-")
                                      nil ".typescript"))
      (unwind-protect
          (with-typescript
            (with-temp-buffer
              (insert typescript--typescript-inserter)
              (insert "(")
              (insert (json-encode-list defun-info))
              (insert ",\n")
              (insert defun-body)
              (insert "\n)")
              (write-region (point-min) (point-max) temp-name
                            nil 1))

            ;; Give Mozilla responsibility for deleting this file
            (let* ((content-window (typescript--typescript-content-window
                                    (typescript--get-typescript-context)))
                   (content-document (typescript< content-window "document"))
                   (head (if (typescript? (typescript< content-document "body"))
                             ;; Regular content
                             (typescript< (typescript! (content-document "getElementsByTagName")
                                       "head")
                                  0)
                           ;; Chrome
                           (typescript< content-document "documentElement")))
                   (elem (typescript! (content-document "createElementNS")
                              "http://www.w3.org/1999/xhtml" "script")))

              (typescript! (elem "setAttribute") "type" "text/typescript")
              (typescript! (elem "setAttribute") "src"
                   (format "resource://typescript/%s"
                           (file-name-nondirectory temp-name)))

              (typescript! (head "appendChild") elem)

              (typescript! (content-window "addEventListener") "unload"
                   (typescript! ((typescript-new
                          "Function" "file"
                          "return function() { file.remove(false) }"))
                        (typescript--make-nsilocalfile temp-name))
                   'false)
              (setq temp-name nil)



              ))

        ;; temp-name is set to nil on success
        (when temp-name
          (delete-file temp-name))))))

;;; Main Function

(defalias 'typescript-parent-mode
  (if (fboundp 'prog-mode) 'prog-mode 'fundamental-mode))

;;;###autoload
(define-derived-mode typescript-mode typescript-parent-mode "typescript"
  "Major mode for editing typescript.

Key bindings:

\\{typescript-mode-map}"

  :group 'typescript
  :syntax-table typescript-mode-syntax-table

  (set (make-local-variable 'indent-line-function) 'typescript-indent-line)
  (set (make-local-variable 'beginning-of-defun-function)
       'typescript-beginning-of-defun)
  (set (make-local-variable 'end-of-defun-function)
       'typescript-end-of-defun)

  (set (make-local-variable 'open-paren-in-column-0-is-defun-start) nil)
  (set (make-local-variable 'font-lock-defaults)
       (list typescript--font-lock-keywords
	     nil nil nil nil
	     '(font-lock-syntactic-keywords
               . typescript-font-lock-syntactic-keywords)))

  (set (make-local-variable 'parse-sexp-ignore-comments) t)
  (set (make-local-variable 'parse-sexp-lookup-properties) t)
  (set (make-local-variable 'which-func-imenu-joiner-function)
       #'typescript--which-func-joiner)

  ;; Comments
  (setq comment-start "// ")
  (setq comment-end "")
  (set (make-local-variable 'fill-paragraph-function)
       'typescript-c-fill-paragraph)

  ;; Parse cache
  (add-hook 'before-change-functions #'typescript--flush-caches t t)

  ;; Frameworks
  (typescript--update-quick-match-re)

  ;; Imenu
  (setq imenu-case-fold-search nil)
  (set (make-local-variable 'imenu-create-index-function)
       #'typescript--imenu-create-index)

  (setq major-mode 'typescript-mode)
  (setq mode-name "typescript")

  ;; for filling, pretend we're cc-mode
  (setq c-comment-prefix-regexp "//+\\|\\**"
        c-paragraph-start "$"
        c-paragraph-separate "$"
        c-block-comment-prefix "* "
        c-line-comment-starter "//"
        c-comment-start-regexp "/[*/]\\|\\s!"
        comment-start-skip "\\(//+\\|/\\*+\\)\\s *")

  (let ((c-buffer-is-cc-mode t))
    ;; FIXME: These are normally set by `c-basic-common-init'.  Should
    ;; we call it instead?  (Bug#6071)
    (make-local-variable 'paragraph-start)
    (make-local-variable 'paragraph-separate)
    (make-local-variable 'paragraph-ignore-fill-prefix)
    (make-local-variable 'adaptive-fill-mode)
    (make-local-variable 'adaptive-fill-regexp)
    (c-setup-paragraph-variables))

  (set (make-local-variable 'syntax-begin-function)
       #'typescript--syntax-begin-function)

  ;; Important to fontify the whole buffer syntactically! If we don't,
  ;; then we might have regular expression literals that aren't marked
  ;; as strings, which will screw up parse-partial-sexp, scan-lists,
  ;; etc. and and produce maddening "unbalanced parenthesis" errors.
  ;; When we attempt to find the error and scroll to the portion of
  ;; the buffer containing the problem, JIT-lock will apply the
  ;; correct syntax to the regular expresion literal and the problem
  ;; will mysteriously disappear.
  (font-lock-set-defaults)

  (let (font-lock-keywords)         ; leaves syntactic keywords intact
    ;; Avoid byte-compilation errors.  `font-lock-fontify-buffer' is
    ;; marked as interactive only in Emacs 25.
    (with-no-warnings
      (font-lock-fontify-buffer)))

  (run-mode-hooks 'typescript-mode-hook))

;;;###autoload
(eval-after-load 'folding
  '(when (fboundp 'folding-add-to-marks-list)
     (folding-add-to-marks-list 'typescript-mode "// {{{" "// }}}" )))

(provide 'typescript)

;; arch-tag: 1a0d0409-e87f-4fc7-a58c-3731c66ddaac
;;; typescript.el ends here
