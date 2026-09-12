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

DOT_EMACS supports GNU Emacs 23.4 and later.

The compatibility floor, the policy for changing that floor, and the
distinction between the stable core and version-gated enhancements are
maintained in [doc/GUIDELINES](doc/GUIDELINES).

The project is used on Debian/Ubuntu, other Linux distributions, and macOS.
Platform- and version-specific behavior configured by DOT_EMACS is documented
in [doc/FEATURES.md](doc/FEATURES.md).

A version named in a validation record or release history is evidence for
that point in time, not an upper support boundary. This README therefore does
not maintain a current newest-supported GNU Emacs or Debian release.

---

## 3. Installation

Run the `install_dotemacs.sh` script to install DOT_EMACS:

### Usage:

```bash
./install_dotemacs.sh [options] [emacs_binary] [target_path] [nosudo]
```

Arguments:

- `[emacs_binary]`: Path to the Emacs binary (default: `emacs`).
- `[target_path]`: Installation directory path (default:
  `/usr/local/etc/emacs.d`). The supported install model expects a directory
  path rather than a directory symlink used as the target itself. The value is
  ignored by `--uninstall`, but keep it as a placeholder when passing
  `[nosudo]`.
- `[nosudo]`: If specified, the script runs without `sudo`.

Options:

- `-h`, `--help`: Show the help message and exit.
- `-v`, `--version`: Show the script header and exit.
- `-u`, `--uninstall`: Remove the user configuration and the fixed default
  installation target. Custom installation targets are not removed.
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

A custom target is an installation directory path. Using a directory symlink as
the installation target itself is outside the supported installation model.

### Environment-Specific Configuration:

At install time, DOT_EMACS uses environment-specific files from
`~/etc/config.local/` when they exist:

- `dot_mew.el` replaces the installed `~/.mew.el`;
- `proxy.el`, `emacs-w3m.el`, and `faces.el` replace their deployed files under
  the installation target's `elisp/` directory.

`proxy.el` is the shared environment-level proxy configuration for environments
such as corporate networks that require a proxy. Its values are expected to be
set before the relevant network integrations are loaded; this is startup
configuration, not a live session reconfiguration interface.

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
Custom installation targets are not tracked for later removal and are not
removed automatically.

`--uninstall` shares the environment setup with the installer, so it still requires a usable Emacs binary. Remove the configuration before removing Emacs itself.

---

## 4. Default Behavior

DOT_EMACS:

- Configures Emacs with selected bundled third-party libraries.
- Loads the installed DOT_EMACS configuration from ~/.emacs.d/elisp/, starting with init.el and its configured modules.
- Persists minibuffer history and the kill ring across Emacs sessions using the built-in `savehist`, which autosaves every 300 seconds; `savekill` also saves the kill ring immediately on every update.
- Language-specific packages not bundled with DOT_EMACS must be installed and configured separately.
- Uses `/dev/shm` as the temporary file directory on GNU/Linux only. Other platforms, including macOS, keep the Emacs default.
- Does not define a separate DOT_EMACS `custom-file`; user-maintained local
  overrides belong in `~/.emacs.d/site-lisp/loader.el`.
- Loads `~/.emacs.d/site-lisp/loader.el` after the bundled configuration when
  that file exists, and stays quiet when it is absent.

For a complete reference to the DOT_EMACS key bindings, mode-specific
shortcuts and automatically enabled behavior, see [FEATURES](doc/FEATURES.md).

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
│   └── site-lisp/            User-local extension directory. loader.el, when present,
│                             is loaded after the bundled DOT_EMACS configuration.
└── doc/
    ├── FEATURES.md           User-facing reference for key bindings and automatically enabled behavior.
    ├── GUIDELINES            Coding style and Emacs Lisp compatibility policy.
    ├── VERSIONS              Version history of the repository.
    ├── LICENSE               License notice.
    ├── COPYING               GPL version 3 text.
    └── COPYING.LESSER        LGPL version 3 text.
```

Only `emacs.d/elisp/` is deployed to the installation target (by default
`/usr/local/etc/emacs.d/elisp`, symlinked as `~/.emacs.d/elisp`). The installer
creates the other directories in the user's home directory. `site-lisp` is the
user-local extension tree; `loader.el`, when present, is loaded after the bundled
configuration. The other directories hold writable run-time state such as
backups, temporary files and history, while the system-wide configuration tree
stays read-only.

Within `emacs.d/elisp/`, the bundled loading order is `init.el` →
`autoloads.el` → each module → `configs.el`; the optional user-local
`~/.emacs.d/site-lisp/loader.el` is loaded after that sequence. A new bundled
setting normally becomes a new `<name>-settings.el` file plus one `load` line in
`autoloads.el`. Bundled settings that must win over earlier bundled modules
belong in `configs.el`. User-maintained local overrides belong in
`site-lisp/loader.el`. See [GUIDELINES](doc/GUIDELINES) for the compatibility
rules these files follow.

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
