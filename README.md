[![MELPA](https://melpa.org/packages/macports-badge.svg)](https://melpa.org/#/macports)

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

First you probably want to have [MacPorts](https://www.macports.org) installed.

You can install macports.el from [MELPA](https://melpa.org/#/flutter) with
`package.el`.

[Get started with MELPA](https://melpa.org/#/getting-started), then run:

```
M-x package-install macports
```

# Configuration

The following variables can be customized in the usual ways (including `M-x
customize`):

- `macports-command` (default: `"port"`)

     The MacPorts binary.
  [exec-path-from-shell](https://github.com/purcell/exec-path-from-shell) is a
  great way to make sure Emacs can find your `port` binary, but alternatively
  you can modify this variable.
- `macports-show-status` (default: `t`)

    Whether to show port counts in the main `macports` transient.
- `macports-use-sudo`: (default: `t`)

    Whether to use `sudo` when invoking `macports-command`. Set this to `nil` if
  you have custom MacPorts installation owned by your user.

# Usage

Execute `M-x macports` to open the main entrypoint.

# License

GPL-3
