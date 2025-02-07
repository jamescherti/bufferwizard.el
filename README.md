# bufferwizard.el
![Build Status](https://github.com/jamescherti/bufferwizard.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/bufferwizard.el)
![](https://raw.githubusercontent.com/jamescherti/bufferwizard.el/main/.images/made-for-gnu-emacs.svg)

The **bufferwizard** Emacs package offers a suite of helper functions, including efficient tools for deleting and renaming files. It also ensures that all associated buffers, including indirect buffers, are properly handled during these operations.

The current version includes:
- `(bufferwizard-rename-file)`: Renames the file that the current buffer is visiting. This command updates the file name on disk, adjusts the buffer name, and updates any indirect buffers or other buffers associated with the old file.
- `(bufferwizard-delete-file)`: Delete the file associated with a buffer and kill all buffers visiting the file, including indirect buffers or clones.
- `(bufferwizard-clear-highlights)`: Clear highlights and related state in the buffer (e.g., clear lazy highlights).
- `(bufferwizard-toggle-highlight-symbol-at-point)`: Toggle highlighting for the symbol at point. This function checks if the symbol at point is currently highlighted. If it is, it removes the highlight; otherwise, it applies the highlight.
- `(bufferwizard-replace-symbol-at-point)`: Replace occurrences of a symbol at point with a specified string.

## Installation

### Install with straight (Emacs version < 30)

To install `bufferwizard` with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package bufferwizard
  :ensure t
  :commands (bufferwizard-rename-file
             bufferwizard-delete-file)
  :straight (bufferwizard
             :type git
             :host github
             :repo "jamescherti/bufferwizard.el"))
```

### Installing with use-package and :vc (Built-in feature in Emacs version >= 30)

To install `bufferwizard` with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package bufferwizard
  :demand t
  :commands (bufferwizard-rename-file
             bufferwizard-delete-file)
  :vc (:url "https://github.com/jamescherti/bufferwizard.el"
       :rev :newest))
```

## Author and License

The `bufferwizard` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [bufferwizard.el @GitHub](https://github.com/jamescherti/bufferwizard.el)
