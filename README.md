# Tide

*TypeScript Interactive Development Environment for Emacs*

[screencast](http://i.imgur.com/jEwgPsd.gif)

### Installation

* Install [node.js](https://nodejs.org/) v0.12.0 or greater.
* Tide is available in [melpa](http://melpa.org/#/tide). You can
  install tide via package-install <kbd>M-x package-install [ret] tide</kbd>

```cl
;; sample config
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode +1)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode +1)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions t :placeOpenBraceOnNewLineForFunctions nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

;; Tide can be used along with web-mode to edit tsx files
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (tide-setup)
              (flycheck-mode +1)
              (setq flycheck-check-syntax-automatically '(save mode-enabled))
              (eldoc-mode +1)
              (company-mode))))

```

### Notes

* Make sure to add
  [tsconfig.json](https://github.com/Microsoft/TypeScript/wiki/tsconfig.json)
  in the project root folder.

* tsserver mangles output
  sometimes [issue - #2758](https://github.com/Microsoft/TypeScript/issues/2758),
  which will result in json parse error. Try node version 0.12.x if
  you get this error.


### Commands

Keyboard shortcuts                  | Description
------------------------------------|----------
<kbd>C-c d</kbd>                    | Show documentation for the symbol at point.
<kbd>M-.</kbd>                      | Jump to the definition of the symbol at point. With a prefix arg, Jump to the type definition.
<kbd>M-,</kbd>                      | Return to your pre-jump position.

<kbd>M-x tide-restart-server</kbd> Restart tsserver. Currently
tsserver doesn't pickup tsconfig.json file changes. This would come in
handy after you edit tsconfig.json.

<kbd>M-x tide-references</kbd> List all references to the symbol
at point in a buffer. References can be navigated using <kbd>n</kbd>
and <kbd>p</kbd>. Press <kbd>enter</kbd> to open the file.

<kbd>M-x tide-rename-symbol</kbd> Rename all occurrences of the symbol
at point.

<kbd>M-x tide-format</kbd> Format the current region or buffer.

### Features

* ElDoc
* Auto complete
* Flycheck
* Jump to definition, Jump to type definition
* Find occurrences
* Rename symbol
* Imenu

### Debugging

Tide uses
[tsserver](https://github.com/Microsoft/TypeScript/pull/2041) as the
backend for most of the features. It writes out a comprehensive log
file which can be captured by setting
`tide-tsserver-process-environment` variable.

```lisp
(setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /tmp/tss.log"))
```
