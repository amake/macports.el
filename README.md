[![MELPA](https://melpa.org/packages/flutter-badge.svg)](https://melpa.org/#/macports)

# macports.el

An Emacs porcelain for [MacPorts](https://www.macports.org).

<!--
    Recorded with asciinema and converted to SVG with svg-term-cli
    https://asciinema.org/
    https://github.com/marionebl/svg-term-cli
-->
![macports.el demo](./docs/macports.el.svg)

# Features

Interactively

- Perform basic MacPorts functions like install, selfupdate, and reclaim
- Manage outdated and installed ports
- Manage `select` selections

with an intuitive [Transient](https://github.com/magit/transient)-based
interface.

# Installation

You can install from [MELPA](https://melpa.org/#/flutter) with `package.el`.

First [get started with MELPA](https://melpa.org/#/getting-started), then run:

```
M-x package-install macports
```

# Usage

Execute `M-x macports` to open the main entrypoint.

# License

GPL-3
