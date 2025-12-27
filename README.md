# guava-themes (WIP)
A few Emacs themes inspired by guavas (and other plants).

<img src="https://raw.githubusercontent.com/bormoge/guava-themes/refs/heads/images/logo/guava-flower-drawing-2.png" width="300" height="300" alt="guava-drawing">
<!-- ![guava-drawing](./img/guava-flower-drawing-2.png) -->

### Table of Contents
- [Introduction](#introduction)
- [Installation](#installation)
- [Screenshots](#screenshots)
- [Contributions](#contributions)
- [License](#license)

## Introduction
This package began as an specific idea: *what if I could create an Emacs theme based on my favorite fruit? And what if it was a light theme, but not **too light**?*  
So here it is. My first Emacs theme(s) / package.  
Although I intend for each commit to be at least on a working state, for now this is a WIP project so you should expect things to go not so smoothly.

## Installation
- Download the repository into ~/.emacs.d/elpa/  
I recommend you use this Emacs code:
```
(use-package guava-themes
  :vc (:url "https://github.com/bormoge/guava-themes"
            :rev :newest
            :branch "main"
            :vc-backend Git)
  :ensure t
  :config
  (setq ring-bell-function #'guava-change-visible-bell)
  (setq visible-bell t)
  (load-theme 'guava-psidium t)
  )
```

## Screenshots
- Guava Psidium Theme  

<img src="https://raw.githubusercontent.com/bormoge/guava-themes/refs/heads/images/screenshots/guava-psidium-theme.png" width="780" height="400" alt="guava-psidium-screenshot">

- Guava Prunus Theme  

TBC

- Guava Jacaranda Theme  

TBC

## Contributions
Issues, pull requests, and forks are welcome.

Here is a tentative list of things to consider when contributing:


* When adding or modifying something that changes the visuals of Emacs (e.g. a face), make sure to include screenshots of the original and changed versions.
* When making a pull request, make sure the commits adhere to the [Conventional Commits](https://www.conventionalcommits.org/en/v1.0.0/) specification.
* Make sure commits are modular; one feat/fix/doc/etc at a time.

## License

SPDX-License-Identifier: GPL-3.0-or-later

This program is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
