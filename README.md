# Tide

[![Build Status](https://api.travis-ci.org/ananthakumaran/tide.svg?branch=master)](https://travis-ci.org/ananthakumaran/tide)
[![Code Quality: Javascript](https://img.shields.io/lgtm/grade/javascript/g/ananthakumaran/tide.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/ananthakumaran/tide/context:javascript)
[![Total Alerts](https://img.shields.io/lgtm/alerts/g/ananthakumaran/tide.svg?logo=lgtm&logoWidth=18)](https://lgtm.com/projects/g/ananthakumaran/tide/alerts)

*TypeScript Interactive Development Environment for Emacs*

[screencast](http://i.imgur.com/jEwgPsd.gif)

### Installation

* Install [node.js](https://nodejs.org/) v0.12.0 or greater.
* Make sure
  [tsconfig.json](http://www.typescriptlang.org/docs/handbook/tsconfig-json.html)
  or
  [jsconfig.json](https://code.visualstudio.com/docs/languages/jsconfig)
  is present in the root folder of the project.
* Tide is available in [melpa](http://melpa.org/#/tide). You can
  install tide via package-install <kbd>M-x package-install [ret] tide</kbd>

### Configuration

#### TypeScript
```elisp
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

(add-hook 'typescript-mode-hook #'setup-tide-mode)
```

#### Format options

Format options can be specified in multiple ways.

* via elisp

```elisp
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
```

* via tsfmt.json (should be present in the root folder along with tsconfig.json)
```json
{
  "indentSize": 4,
  "tabSize": 4,
  "insertSpaceAfterOpeningAndBeforeClosingTemplateStringBraces": false,
  "placeOpenBraceOnNewLineForFunctions": false,
  "placeOpenBraceOnNewLineForControlBlocks": false
}
```
Check [here][format_options] for the full list of supported format options.


#### TSX
```elisp
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)
```

Tide also provides support for editing js & jsx files. Tide checkers
`javascript-tide` and `jsx-tide` are not enabled by default for js &
jsx files. It can be enabled by setting [`flycheck-checker`](http://www.flycheck.org/en/latest/user/syntax-checkers.html#variable-flycheck-checker)

#### JavaScript

Create `jsconfig.json` in the root folder of your project.
`jsconfig.json` is `tsconfig.json` with `allowJs` attribute set to
true.

```json
{
  "compilerOptions": {
    "target": "es2017",
    "allowSyntheticDefaultImports": true,
    "noEmit": true,
    "checkJs": true,
    "jsx": "react",
    "lib": [ "dom", "es2017" ]
  }
}
```


```elisp
(add-hook 'js2-mode-hook #'setup-tide-mode)
;; configure javascript-tide checker to run after your default javascript checker
(flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
```

#### JSX
```elisp
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
(flycheck-add-mode 'javascript-eslint 'web-mode)
(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)
```

#### Use Package
```elisp
(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-format-before-save)))
```

### Commands

Keyboard shortcuts                  | Description
------------------------------------|----------
<kbd>M-.</kbd>                      | Jump to the definition of the symbol at point. With a prefix arg, Jump to the type definition.
<kbd>M-,</kbd>                      | Return to your pre-jump position.

<kbd>M-x tide-restart-server</kbd> Restart tsserver. This would come
in handy after you edit tsconfig.json or checkout a different branch.

<kbd>M-x tide-documentation-at-point</kbd> Show documentation for the
symbol at point.

<kbd>M-x tide-references</kbd> List all references to the symbol
at point in a buffer. References can be navigated using <kbd>n</kbd>
and <kbd>p</kbd>. Press <kbd>enter</kbd> to open the file.

<kbd>M-x tide-project-errors</kbd> List all errors in the
project. Errors can be navigated using <kbd>n</kbd> and
<kbd>p</kbd>. Press <kbd>enter</kbd> to open the file.

<kbd>M-x tide-rename-symbol</kbd> Rename all occurrences of the symbol
at point.

<kbd>M-x tide-rename-file</kbd> Rename current file and all it's
references in other files.

<kbd>M-x tide-format</kbd> Format the current region or buffer.

<kbd>M-x tide-fix</kbd> Apply code fix for the error at point. When
invoked with a prefix arg, apply code fix for all the errors in the file
that are similar to the error at point.

<kbd>M-x tide-add-tslint-disable-next-line</kbd> If the point is on
one or more tslint errors, add a `tslint:disable-next-line` flag on
the previous line to silence the errors. Or, if a flag already exists
on the previous line, modify the flag to silence the errors.

<kbd>M-x tide-refactor</kbd> Refactor code at point or current region.

<kbd>M-x tide-jsdoc-template</kbd> Insert JSDoc comment template at
point.

<kbd>M-x tide-verify-setup</kbd> Show the version of tsserver.

<kbd>M-x tide-organize-imports</kbd> Organize imports in the file.

### Features

* ElDoc
* Auto complete
* Flycheck
* Jump to definition, Jump to type definition
* Find occurrences
* Rename symbol
* Imenu
* Compile On Save
* Highlight Identifiers
* Code Fixes
* Code Refactor
* Organize Imports

### Debugging

![architecture](https://github.com/ananthakumaran/tide/raw/master/doc/architecture.mmd.png)

Tide uses
[tsserver](https://github.com/Microsoft/TypeScript/wiki/Standalone-Server-%28tsserver%29) as the
backend for most of the features. It writes out a comprehensive log
file which can be captured by setting
`tide-tsserver-process-environment` variable.

```lisp
(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
```


[format_options]: https://github.com/Microsoft/TypeScript/blob/87e9506/src/services/services.ts#L1244-L1272

### FAQ?

**How do I configure tide to use a specific version of TypeScript compiler?**

For TypeScript 2.0 and above, you can customize the
`tide-tsserver-executable` variable. For example
```lisp
(setq tide-tsserver-executable "node_modules/typescript/bin/tsserver")
```

Sadly, this won't work for TypeScript < 2.0. You can clone the repo
locally and checkout the old version though.

### Custom Variables

**tide-sync-request-timeout** `2`

The number of seconds to wait for a sync response.

**tide-tsserver-process-environment** `'nil`

List of extra environment variables to use when starting tsserver.

**tide-tsserver-executable** `nil`

Name of tsserver executable to run instead of the bundled tsserver.

This may either be an absolute path or a relative path. Relative
paths are resolved against the project root directory.

Note that this option only works with TypeScript version 2.0 and
above.

**tide-node-executable** `"node"`

Name or path of the node executable binary file.

**tide-post-code-edit-hook** `nil`

Hook run after code edits are applied in a buffer.

**tide-sort-completions-by-kind** `nil`

Whether completions should be sorted by kind.

**tide-format-options** `'nil`

Format options plist.

**tide-user-preferences** `'(:includeCompletionsForModuleExports t :includeCompletionsWithInsertText t :allowTextChangesInNewFiles t)
`

User preference plist used on the configure request.

Check
https://github.com/Microsoft/TypeScript/blob/17eaf50b73c1355d2fd15bdc3912aa64a73483dd/src/server/protocol.ts#L2684
for the full list of available options.

**tide-completion-ignore-case** `nil`

CASE will be ignored in completion if set to non-nil.

**tide-completion-detailed** `nil`

Completion dropdown will contain detailed method information if set to non-nil.

**tide-completion-enable-autoimport-suggestions** `t`

Whether to include external module exports in completions.

**tide-jump-to-definition-reuse-window** `t`

Reuse existing window when jumping to definition.

**tide-imenu-flatten** `nil`

Imenu index will be flattened if set to non-nil.

**tide-allow-popup-select** `'(code-fix refactor)
`

The list of commands where popup selection is allowed.

**tide-always-show-documentation** `nil`

Show the documentation window even if only type information is available.

**tide-server-max-response-length** `102400`

Maximum allowed response length from tsserver. Any response greater than this would be ignored.

**tide-tsserver-locator-function** `#'tide-tsserver-locater-npmlocal-projectile-npmglobal`

Function used by tide to locate tsserver.

**tide-default-mode** `"TS"`

The default mode to open buffers not backed by files (e.g. Org
  source blocks) in.

**tide-recenter-after-jump** `t`

Recenter buffer after jumping to definition

**tide-filter-out-warning-completions** `nil`

Completions whose `:kind` property is "warning" will be filtered out if set to non-nil.
This option is useful for Javascript code completion, because tsserver often returns a lot of irrelevant
completions whose `:kind` property is "warning" for Javascript code. You can fix this behavior by setting
this variable to non-nil value for Javascript buffers using `setq-local` macro.

**tide-hl-identifier-idle-time** `0.5`

How long to wait after user input before highlighting the current identifier.

