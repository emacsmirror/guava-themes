[![MELPA](https://melpa.org/packages/guava-themes-badge.svg)](https://melpa.org/#/guava-themes)

# guava-themes
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
So here it is. My first Emacs package / pack of themes.  
Originally conceived as a tribute to the [guava fruit](https://en.wikipedia.org/wiki/Guava), this pack of themes ended up being inspired by different types of plants I found eye-catching.  
And so, the package name became an Artifact Title, as is known in the tropes community.  

## Installation
- I recommend you use this Emacs code to automatically install **guava-themes** from [MELPA](https://melpa.org/).  
```
(use-package guava-themes
  :ensure t
  :config
  (setq ring-bell-function #'guava-themes-change-visible-bell)
  (setq visible-bell t)
  (load-theme 'guava-themes-psidium t)
  )
```
- You can also download this package directly from the source. This code should work on Emacs 30:  
```
(use-package guava-themes
  :vc (:url "https://github.com/bormoge/guava-themes"
            :rev :newest
            :branch "main"
            :vc-backend Git)
  :ensure t
  :config
  (setq ring-bell-function #'guava-themes-change-visible-bell)
  (setq visible-bell t)
  (load-theme 'guava-themes-psidium t)
  )
```
- If the above code doesn't work you can also try using this one:  
```
(unless (package-installed-p 'guava-themes)
  (package-vc-install "https://github.com/bormoge/guava-themes" nil nil 'guava-themes))

(require 'guava-themes)

(setq ring-bell-function #'guava-themes-change-visible-bell)
(setq visible-bell t)
(load-theme 'guava-themes-psidium t)
```

## Screenshots
- Guava Psidium Theme  

<img src="https://raw.githubusercontent.com/bormoge/guava-themes/refs/heads/images/screenshots/guava-psidium-theme.png" width="780" height="400" alt="guava-psidium-screenshot">

- Guava Jacaranda Theme  

<img src="https://raw.githubusercontent.com/bormoge/guava-themes/refs/heads/images/screenshots/guava-jacaranda-theme.png" width="780" height="400" alt="guava-jacaranda-screenshot">

- Guava Prunus Theme  

<img src="https://raw.githubusercontent.com/bormoge/guava-themes/refs/heads/images/screenshots/guava-prunus-theme.png" width="780" height="400" alt="guava-prunus-screenshot">

- Guava Dracaena Theme  

<img src="https://raw.githubusercontent.com/bormoge/guava-themes/refs/heads/images/screenshots/guava-dracaena-theme.png" width="780" height="400" alt="guava-dracaena-screenshot">

- Guava Acer Theme  

<img src="https://raw.githubusercontent.com/bormoge/guava-themes/refs/heads/images/screenshots/guava-acer-theme.png" width="780" height="400" alt="guava-acer-screenshot">

- Guava Cordyline Theme  

<img src="https://raw.githubusercontent.com/bormoge/guava-themes/refs/heads/images/screenshots/guava-cordyline-theme.png" width="780" height="400" alt="guava-cordyline-screenshot">

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
