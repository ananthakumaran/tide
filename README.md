# Tide

*TypeScript Interactive Development Environment for Emacs*

[screencast](http://i.imgur.com/jEwgPsd.gif)

### Installation

* Install [node.js](https://nodejs.org/) v0.12.0 or greater.
* Tide is available in [melpa](http://melpa.org/#/tide). You can
  install tide via package-install <kbd>M-x package-install [ret] tide</kbd>

````cl
;; sample config
(add-hook 'typescript-mode-hook
          (lambda ()
            (tide-setup)
            (flycheck-mode t)
            (setq flycheck-check-syntax-automatically '(save mode-enabled))
            (eldoc-mode t)
            ;; company is an optional dependency. You have to
            ;; install it separately via package-install
            (company-mode-on)))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

````

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


### Features

* ElDoc
* Auto complete
* Flycheck
* Jump to definition, Jump to type definition
* Find occurrences
* Rename symbol
* Imenu
