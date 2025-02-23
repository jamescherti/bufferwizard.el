# bufferwizard.el
![Build Status](https://github.com/jamescherti/bufferwizard.el/actions/workflows/ci.yml/badge.svg)
![License](https://img.shields.io/github/license/jamescherti/bufferwizard.el)
![](https://raw.githubusercontent.com/jamescherti/bufferwizard.el/main/.images/made-for-gnu-emacs.svg)

The **bufferwizard** Emacs package offers a suite of helper functions:
- `(bufferwizard-toggle-highlight-symbol-at-point)`: Toggle highlighting for the symbol at point. This function checks if the symbol at point is currently highlighted. If it is, it removes the highlight; otherwise, it applies the highlight. (This is a lightweight alternative to the `highlight-symbol` package.) The variable `case-fold-search` also affects the behavior of symbol highlighting. When `case-fold-search` is set to `t`, symbol highlighting will be case-insensitive, meaning it will match symbols regardless of whether they are upper or lower case. Conversely, when `case-fold-search` is set to `nil`, symbol highlighting becomes case-sensitive, matching only the exact case of the symbol as it appears in the buffer.
- `(bufferwizard-replace-symbol-at-point)`: Replace occurrences of a symbol at point with a specified string. Case sensitivity can be controlled by modifying the `case-fold-search` variable. Setting it to `nil` makes searches case-sensitive, while setting it to `t` makes them case-insensitive. This setting applies to all search operations, including `query-replace-regexp`.
- `(bufferwizard-clone-indirect-buffer)` and `(bufferwizard-clone-and-switch-to-indirect-buffer)`: These functions are enhanced versions of the built-in `clone-indirect-buffer`. They create an indirect buffer with the same content as the current buffer while preserving the point position, window start, and horizontal scroll position. This package also provides the `(bufferwizard-switch-to-base-buffer)` function, which allows switching from an indirect buffer to its corresponding base buffer.

## Installation

### Install with straight (Emacs version < 30)

To install `bufferwizard` with `straight.el`:

1. It if hasn't already been done, [add the straight.el bootstrap code](https://github.com/radian-software/straight.el?tab=readme-ov-file#getting-started) to your init file.
2. Add the following code to the Emacs init file:
```emacs-lisp
(use-package bufferwizard
  :ensure t
  :straight (bufferwizard
             :type git
             :host github
             :repo "jamescherti/bufferwizard.el"))
```

### Installing with use-package and :vc (Built-in feature in Emacs version >= 30)

To install `bufferwizard` with `use-package` and `:vc` (Emacs >= 30):

``` emacs-lisp
(use-package bufferwizard
  :ensure t
  :vc (:url "https://github.com/jamescherti/bufferwizard.el"
       :rev :newest))
```

## Author and License

The `bufferwizard` Emacs package has been written by [James Cherti](https://www.jamescherti.com/) and is distributed under terms of the GNU General Public License version 3, or, at your choice, any later version.

Copyright (C) 2024-2025 James Cherti

This program is free software: you can redistribute it and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details. You should have received a copy of the GNU General Public License along with this program.

## Links

- [bufferwizard.el @GitHub](https://github.com/jamescherti/bufferwizard.el)
