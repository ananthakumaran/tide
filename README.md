# Tide

*TypeScript Interactive Development Environment for Emacs*

[screencast](http://i.imgur.com/jEwgPsd.gif)

### Installation

* Install [node.js](https://nodejs.org/) v0.12.0 or greater.
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
```

Tide also provides support for editing js & jsx files. Tide checkers
`javascript-tide` and `jsx-tide` are not enabled by default for js &
jsx files. It can be enabled by setting [`flycheck-checker`](http://www.flycheck.org/en/latest/user/syntax-checkers.html#variable-flycheck-checker)

#### JavaScript
```elisp
(add-hook 'js2-mode-hook #'setup-tide-mode)
```

#### JSX
```elisp
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "jsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
```

### Notes

* Make sure
  [tsconfig.json](http://www.typescriptlang.org/docs/handbook/tsconfig-json.html)
  or
  [jsconfig.json](https://code.visualstudio.com/docs/languages/jsconfig)
  is present in the root folder of the project.

* tsserver mangles output
  sometimes [issue - #2758](https://github.com/Microsoft/TypeScript/issues/2758),
  which will result in json parse error. Try node version 0.12.x if
  you get this error.


### Commands

Keyboard shortcuts                  | Description
------------------------------------|----------
<kbd>M-.</kbd>                      | Jump to the definition of the symbol at point. With a prefix arg, Jump to the type definition.
<kbd>M-,</kbd>                      | Return to your pre-jump position.

<kbd>M-x tide-restart-server</kbd> Restart tsserver. Currently
tsserver doesn't pickup tsconfig.json file changes. This would come in
handy after you edit tsconfig.json.

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

<kbd>M-x tide-format</kbd> Format the current region or buffer.

<kbd>M-x tide-fix</kbd> Apply code fix for the error at point.

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

### Debugging

Tide uses
[tsserver](https://github.com/Microsoft/TypeScript/pull/2041) as the
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
