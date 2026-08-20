# DOT_EMACS

A curated and modular Emacs configuration with selected bundled third-party libraries.

---

## Contents

1. [Overview](#1-overview)
2. [Supported Environments](#2-supported-environments)
3. [Installation](#3-installation)
4. [Default Behavior](#4-default-behavior)
5. [Directory Structure](#5-directory-structure)
6. [Versioning](#6-versioning)
7. [Contribution](#7-contribution)
8. [License](#8-license)

---

## 1. Overview

DOT_EMACS is designed to:

- Simplify the setup of a powerful Emacs environment with preconfigured settings.
- Include selected third-party libraries where they remain useful.
- Provide a modular and customizable structure for user preferences.

---

## 2. Supported Environments

DOT_EMACS is confirmed to work on:

- GNU Emacs on Debian/Ubuntu stable (from Debian 7 Wheezy with Emacs 23.4 to Debian 13 Trixie with Emacs 30.1).
- GNU Emacs on other Linux distributions within the same version range.
- Emacs for macOS (from [emacsformacosx.com](http://emacsformacosx.com/)).

Supported Emacs versions:
- GNU Emacs 23.4 and later (up to and including 30.1, with future releases expected to be supported).

---

## 3. Installation

Run the `install_dotemacs.sh` script to install DOT_EMACS:

### Usage:

```bash
./install_dotemacs.sh [options] [emacs_binary] [target_path] [nosudo]
```

Arguments:

- `[emacs_binary]`: Path to the Emacs binary (default: `emacs`).
- `[target_path]`: Installation directory (default: `/usr/local/etc/emacs.d`).
  Ignored by `--uninstall`, but keep it as a placeholder when passing `[nosudo]`.
- `[nosudo]`: If specified, the script runs without `sudo`.

Options:

- `-h`, `--help`: Show the help message and exit.
- `-v`, `--version`: Show the script header and exit.
- `-u`, `--uninstall`: Remove the installed dot_emacs components.
- `-n`, `--no-sudo`: Run without `sudo`.

### Default Installation:

```bash
~/dot_emacs/install_dotemacs.sh
```

This installs DOT_EMACS to the default location. Root privileges (via `sudo`) are required.

### Custom Installation:

```bash
~/dot_emacs/install_dotemacs.sh /usr/bin/emacs ~/.emacs.d nosudo
~/dot_emacs/install_dotemacs.sh --no-sudo /usr/bin/emacs ~/.emacs.d
```

Both commands install DOT_EMACS to `~/.emacs.d` without using `sudo`.

### Installation on macOS:

```bash
~/dot_emacs/install_dotemacs.sh /Applications/Emacs.app/Contents/MacOS/Emacs
```

When the `emacs` command is not found and `[emacs_binary]` is not an executable
path, the script falls back to `/Applications/Emacs.app/Contents/MacOS/Emacs`
if it is executable, so passing the path explicitly is optional.

### Uninstallation:

```bash
~/dot_emacs/install_dotemacs.sh --uninstall
~/dot_emacs/install_dotemacs.sh --uninstall -n
```

The second command removes the installed configuration without using `sudo`.

For safety, `--uninstall` removes only `/usr/local/etc/emacs.d`.
Custom installation targets are not removed automatically.

`--uninstall` shares the environment setup with the installer, so it still requires a usable Emacs binary. Remove the configuration before removing Emacs itself.

---

## 4. Default Behavior

DOT_EMACS:

- Configures Emacs with selected bundled third-party libraries.
- Loads the installed DOT_EMACS configuration from ~/.emacs.d/elisp/, starting with init.el and its configured modules.
- Persists minibuffer history and the kill ring across Emacs sessions using the built-in `savehist`, which autosaves every 300 seconds; `savekill` also saves the kill ring immediately on every update.
- Language-specific packages not bundled with DOT_EMACS must be installed and configured separately.
- Uses `/dev/shm` as the temporary file directory on GNU/Linux only. Other platforms, including macOS, keep the Emacs default.

---

## 5. Directory Structure

This section describes the main directories of the repository and what each one
is for. It is not a complete file listing: `emacs.d/elisp/` alone holds several
dozen files, and only the entries worth knowing about before editing anything
are shown.

```
.
├── dot_emacs                 Installed as ~/.emacs. Loads ~/.emacs.d/elisp/init.el and nothing else.
├── dot_mew.el                Installed as ~/.mew.el. Mew (mail) settings.
├── install_dotemacs.sh       Installer and uninstaller.
├── emacs.d/
│   ├── elisp/                The configuration itself. Copied to the target; selected
│   │                         modules are byte-compiled while bootstrap/orchestration and
│   │                         load-order-sensitive configuration files remain source-loaded.
│   │   ├── init.el           Entry point. Sets paths, then loads autoloads.el.
│   │   ├── autoloads.el      Loads each settings module in order, then configs.el.
│   │   ├── configs.el        Settings applied last, after every module is loaded.
│   │   ├── *-settings.el     One file per package or mode (dired, mew, tramp, auto-complete, ...).
│   │   ├── *-compat-bridge.el  Shims that keep old code loadable on current Emacs.
│   │   └── 3rd-party/        Bundled third-party libraries, including yatex-mode.
│   └── site-lisp/            Placeholder, as are anything/, backups/, tmp/ and the other
│                             run-time directories beside it. See the note below.
└── doc/
    ├── GUIDELINES            Coding style and Emacs Lisp compatibility policy.
    ├── VERSIONS              Version history of the repository.
    ├── LICENSE               License notice.
    ├── COPYING               GPL version 3 text.
    └── COPYING.LESSER        LGPL version 3 text.
```

Only `emacs.d/elisp/` is deployed to the installation target (by default
`/usr/local/etc/emacs.d/elisp`, symlinked as `~/.emacs.d/elisp`). The remaining
directories under `emacs.d/` are empty placeholders: the installer creates the
real ones in the user's home directory, so that a system-wide configuration tree
stays read-only while Emacs still has somewhere to write backups, temporary
files and history.

Within `emacs.d/elisp/`, the loading order is `init.el` → `autoloads.el` →
each module → `configs.el`. A new setting normally becomes a new
`<name>-settings.el` file plus one `load` line in `autoloads.el`. Anything that
must win over the modules belongs in `configs.el`, since it is loaded after them
all. See [GUIDELINES](doc/GUIDELINES) for the compatibility rules these files
follow.

---

## 6. Versioning

DOT_EMACS uses the `<year>.<month>` versioning format starting from version `11.09`.
Example: `24.12`

A third `<patch>` level is appended when a release only corrects an earlier one
in the same month. Example: `25.08.1`

Release versions are independent of the versions recorded in the header of each
executable script, which use a two-level `<major>.<minor>` format.

For detailed version history, please refer to the [VERSIONS](doc/VERSIONS) file.

---

## 7. Contribution

We welcome contributions! Here's how you can help:
1. Fork the repository.
2. Add or improve a feature, or fix an issue.
3. Submit a pull request with clear documentation and changes.

Please ensure your code is well-structured and documented.

For coding style and long-term Emacs Lisp compatibility policy,
see the [GUIDELINES](doc/GUIDELINES) document.

---

## 8. License

The parts of this repository copyrighted by id774 are dual licensed under the [GPL version 3](https://www.gnu.org/licenses/gpl-3.0.html) or the [LGPL version 3](https://www.gnu.org/licenses/lgpl-3.0.html), at your option.
The third-party software bundled under `emacs.d/elisp/3rd-party/` is excluded from this dual license and follows the license conditions of each third-party software.
The same exclusion applies to the individual files that contain third-party derived code.
For full details, please refer to the [LICENSE](doc/LICENSE) file.  See also [COPYING](doc/COPYING) and [COPYING.LESSER](doc/COPYING.LESSER) for the complete license texts.

Thank you for using and contributing to this repository!
