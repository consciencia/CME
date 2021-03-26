# Introduction
CME provide semantically aware code navigation and auto completion for
C and C++ without the need to install any external tools. All is
written in elisp. You don't even need compilation database.

CME acts as an wrapper around CEDET which does all the heavy lifting.
It is responsible for:
* CEDET configuration.
* Correcting CEDET bugs.
* New features atop CEDET infrastructure.

## Features
01) Semantically aware goto definition.
02) Dumb goto definition.
03) Project wide symbol search. Similar to `M-x find-function`.
04) Semantically aware auto completion.
05) Dumb symbol reference lookup.
06) Go to parent class.
07) Go to subclass.
08) Toggle between prototype and implementation.
09) Code folding.
10) Local navigation between tags in buffer.
11) Semantically aware documentation lookup with doxygen support.
12) Locally defined variable rename. Function parameters included.
13) Project detection using simple JSON config file in root.
14) Project build.
15) Linking together several projects so that CME acts on them as
    if they were one.
16) When you write dot and it makes sense to write arrow
    instead, arrow is inserted into buffer.

# Installation
Put following code somewhere in your configuration and restart Emacs.

``` emacs-lisp
(let ((url "https://raw.githubusercontent.com/consciencia/CME/master/cme-install.el")
      (cme-dir (concat user-emacs-directory "CME")))
  (when (not (file-directory-p cme-dir))
    (with-temp-buffer
      (url-insert-file-contents url)
      (eval-buffer)))
  (add-to-list 'load-path cme-dir))
(require 'cme)
(cme-init :configure-keys t)
```

In case you don't like that CME is configuring its keyboard shortcuts in
C and C++ buffers, omit `:configure-keys t` from call to `cme-init`.

You don't need to install any additional tools or packages in order to
get all features of CME. Everything is taken care of internally.

As an auto completion front end is used company mode. CME is not enabling
company in C and C++ buffers automatically in order to not interfere
with user code.

In case you do not use company mode, simply put following code after
CME initialization:

``` emacs-lisp
(add-hook 'c-mode-hook 'company-mode)
(add-hook 'c++-mode-hook 'company-mode)
```

No other configuration related to company mode is required, rest is
handled by CME.

# Usage

## Project Configuration File
CME is acquiring information about project in which currently opened
file reside through JSON config file in project root named
`cme-project.json`. Do not forget to place it in every project where
you intend to use CME.

``` json
{
    "global-includes": [
        "/absolute/path/somewhere",
        ...
    ],
    "local-includes": [
        "/include",
        "../include",
        ...
    ],
    "macro-files": [
        "/absolute/path/to/file.h",
        ...
    ],
    "macro-table": {
        "FOO": "1",
        "BAR": "BAZ",
        ...
    },
    "source-roots": [
        "/absolute/path/somewhere",
        ...
    ],
    "build-dir": "/absolute/path/somewhere",
    "build-cmd": "make"
}
```

* **global-includes**<br>
  External include paths. Only absolute paths are allowed. Do not put
  here paths pointing into current project.
  You can omit this parameter.
* **local-includes**<br>
  Internal include paths. Only relative paths from the project root
  are allowed. Do not put here paths pointing outside current project.
  You can omit this parameter. In that case, CME will use default
  local includes.
* **source-roots**<br>
  Absolute paths to other projects which are used by this project.
  Every referenced project must have `cme-project.json` in its root.
  You may omit this parameter.
* **macro-table**<br>
  Dictionary where keys are macro names and values are macro
  values. This is used for resolving preprocessor directives in
  code. You may omit this parameter.
* **macro-files**<br>
  Enumeration of macro files. Only absolute paths are allowed. CME
  parser (Semantic) is not remembering macro definitions encountered
  in files not listed here in order to speed things up. In case you
  are using some macro magic in your project which makes files
  unparseable without macro expansions, you must place the files where
  your magic is defined here. Please note that during parsing macro
  files, no macro definitions from any other (currently processed
  macro file included) macro files are utilized.
* **build-dir**<br>
  Absolute path to build directory. You may omit this parameter.
  In that case, CME will use project root as build directory.
* **build-cmd**<br>
  Build command. You may omit this parameter. In that case, CME
  will use `make`.

You can use CME even without any project config file. In that
case, directory of currently opened file will be interpreted as
an project root and only relative and system includes will be
resolved.

## Index Management
CME supports two modes of indexation. Lazy and ahead of time.
First require no user interaction, second is done only when user
executes `M-x cme-index-current-project`.

Lazy indexer only parses files when it is required and no tag table
was found in DB. Advantage of this mode is speed because you don't
need to build complete index. Disadvantage is the need to know which
files to parse in order to complete the task. Its not obvious every
time. For example, function implementation may be anywhere in the
project. To combat this, grep (or findstr on windows) is used for
collection of candidate files for parsing. Unfortunately, grep can't
be used every time. When you want to list all symbols in project, you
need DB with these symbols. When you have partially indexed
project, you will get partial list of symbols and no grep will
save you.

Ahead of time indexer will parse everything, store it in DB and then
all CME features works exactly as intended. Because operations are not
interrupted by parsing files, everything is very fast.

CME functions which require complete index for correct behavior has it
noted in their documentation.

## API

* **cme-init**<br>
  Initializes CME. When `:configure-keys t` is passed, following
  keyboard shortcuts are registered in all C and C++ buffers:
  * `M-.` cme-jump
  * `C-.` cme-find-anything
  * `C-r` cme-browse-local-tags
  * `M--` cme-symref
  * `M-,` cme-pop-mark
  * `M-*` cme-doc
  * `C-,` cme-proto-impl-toggle
  * `C--` cme-rename-local-var
  * `M-<next>` cme-next-tag
  * `M-<prior>` cme-previous-tag
  * `M-p` cme-follow-ref-up
  * `M-c` cme-find-subclasses
  * `M-f` cme-fold-tag-toggle
  * `M-d` cme-mark-tag
  * `M-g` cme-reparse-buffer
  * `<tab>` company-indent-or-complete-common
* **cme-jump**<br>
  Performs semantical analysis of the current context and
  jump to definition of symbol under point. When there is
  multiple candidates, user is asked to chose one through
  IDO. When semantical analysis fails, candidates are
  acquired by brute force. Not optimal, but better than
  nothing. This command works as intended even with
  incomplete index.
* **cme-find-anything**<br>
  This function is similar to `find-function`. It enables you to
  interactively search through all symbols in current project and
  its dependencies. Input is taken from minibuffer. You can
  get semantically aware auto completion by hitting `<tab>`.
  Semantical analysis is performed in scope from the location of
  cursor where you invoked this command.
  There are two kinds of allowed input:
  * One symbol. In this case, brute force search is performed across
    current project and all its dependencies. User is asked through
    IDO where to jump.
  * Sequence of symbols delimited by `:: . ->`. In this case, similar
    operation to what `cme-jump` does is performed except it takes
    input from minibuffer instead of cursor neighborhood.
    Of course, scope for analysis is taken from the location of
    cursor where you invoked this command. As an root of symbol
    sequence can be used type or function, you are not limited to
    instances of types from local and global scopes. Because of this
    feature, `cme-find-anything` can be used for workarounding
    limitations of contextual parser.
  This command works as intended even with incomplete index except
  for minibuffer auto completion. With incomplete index, you will
  get incomplete auto completion.
* **cme-browse-local-tags**<br>
  Lists all tags from current buffer through IDO and let user select
  where to jump.
* **cme-symref**<br>
  Finds references for symbol under point. Each symbol search
  has dedicated buffer so you can perform recursive searches.
  Following table describes keyboard shortcuts for controlling
  buffer with `cme-symref` output:
  * `<C-right>` Move point to the next button.
  * `<C-left>` Move point to the previous button.
  * `RET` It will either toggle the section, jump to file, jump
    to symbol definition or jump to reference. What happens is
    dependent on the button which you hit.
    Before jump, `cme-push-mark` is called which means you
    can use `cme-pop-mark` to return back.
  * `+ or - or = or SPC` Toggle current section.
  * `C-+` Expand all sections.
  * `C--` Collapse all sections.
  * `C-r or R` Rename all found references in expanded sections.
  * `M-, or C-q or q` Jump to the location from which `cme-symref`
    was initiated.
  This command works as intended even with incomplete index.
* **cme-doc**<br>
  Shows documentation for symbol under cursor. When multiple
  candidates are found, documentation is scrapped from all of
  them and the longest is displayed to user. You can close
  window with documentation by hitting `q`. When doxygen
  documentation is recognized, parameter types are buttonized
  so you can move cursor into them and then go to their
  definition by hitting `RET`. This command works as intended
  even with incomplete index.
* **cme-proto-impl-toggle**<br>
  Toggles between prototype and implementation. When multiple jump
  candidates exist, it will ask user through IDO where to jump.
  This command works as intended even with incomplete index.
* **cme-pop-mark**<br>
  Every jump saves old position through `cme-push-mark`. By
  executing this command, you jump to previously saved position.
* **cme-next-tag**<br>
  Jumps to next tag in current buffer. What qualify as next tag is
  highly dependent on context. Try it!
* **cme-previous-tag**<br>
  Jumps to previous tag in current buffer. What qualify as previous
  tag is highly dependent on context. Try it!
* **cme-follow-ref-up**<br>
  Jumps to parent tag of current tag. When you are in function
  implementation, you will land in its prototype. When you are
  in prototype which is inside class, you will land in its class.
  When you are in class, you will land at its parent class. When
  multiple classes exist, user is asked through IDO where to jump.
  This command works as intended even with incomplete index.
* **cme-find-subclasses**<br>
  Jumps to subclass of current class. When there are multiple
  classes, asks user through IDO where to jump.
  This command works as intended even with incomplete index.
* **cme-rename-local-var**<br>
  Rename local variable under cursor. It works for function arguments
  too. Upon calling this function, all occurrences of target variable
  become highlighted. You can rename all occurrences by editing any
  single highlighted occurrence. Hit `C-RET` in any highlighted
  occurrence to commit rename.
* **cme-fold-tag-toggle**<br>
  Fold or unfold current tag.
* **cme-mark-tag**<br>
  Mark current tag. From function parameters to name spaces.
* **cme-index-current-project**<br>
  Create index for current project and its dependencies. You will be
  asked whether you want to index system include paths too. Be warned
  that system include paths include `/usr/include/` so be prepared
  for a lots of files.
  On 10th gen i5 roughly 60k lines of code can be indexed per minute.
  Not counting comments and blank lines. Be prepared that template
  heavy code tends to consume more time.
* **cme-reparse-buffer**<br>
  Forces full reparse of current buffer.
* **cme-compile-project**<br>
  Compiles current project using `build-dir` and `build-cmd`
  in `cme-project.json`.
* **cme-list-includes**<br>
  Lists all direct and indirect includes in current buffer. Each
  entry is prefixed with resolution status so you can check which
  includes failed to be resolved due to missing include root.

## Troubleshooting

### Auto completion fails to list all candidates
Probably issue with includes. Look whether include statements are
colored red. When they are, CEDET failed to resolve them. Correct
local or global include roots in your `cme-project.json` and reparse
buffer using `M-x cme-reparse-buffer`.

You can also use `M-x cme-list-includes` for reporting resolution
statuses for all direct and indirect includes in current buffer.

### CEDET doesn't see my local variable
It is either issue in parser or issue in caching mechanism for local
variables. Most of the time, it works as intended, but sometimes it
contains outdated tags. Run `M-x cme-reparse-buffer` to fix it. Please
note that running shortcut bound to `cme-reparse-buffer` is not
sufficient, you must invoke it through `M-x`. No idea why. I will
fix it sometime in future when time will allow me.

### Semantically aware operation doesn't work
Semantic analysis is highly dependent on knowledge of local variables
and contextual parser. Either there is issue with local variable cache
or contextual parser failed to parse the neighborhood of cursor.

First issue is solved by running `M-x cme-reparse-buffer`. Please
note that running shortcut bound to `cme-reparse-buffer` is not
sufficient, you must invoke it through `M-x`.

Second issue is (maybe) solvable by bombarding me with complaints.
I plan to rework contextual parser in future. Meanwhile, you can
use `cme-find-anything` as an workaround.
