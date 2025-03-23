# DOT\_EMACS

A pluggable and comprehensive configuration for Emacs that includes third-party libraries to create an all-in-one Emacs environment.

---

## Contents

1. [Overview](#1-overview)
2. [Supported Environments](#2-supported-environments)
3. [Installation](#3-installation)
4. [Default Behavior](#4-default-behavior)
5. [Versioning](#5-versioning)
6. [Contribution](#6-contribution)
7. [License](#7-license)

---

## 1. Overview

DOT\_EMACS is designed to:

- Simplify the setup of a powerful Emacs environment with preconfigured settings.
- Include third-party libraries for enhanced functionality.
- Provide a modular and customizable structure for user preferences.

---

## 2. Supported Environments

DOT_EMACS is confirmed to work on:

- Emacs on Debian/Ubuntu stable.
- Emacs built from source (23.4 or later).
- Emacs for macOS (from [emacsformacosx.com](http://emacsformacosx.com/)).
- Meadow based on Emacs 22.3.1 or later on Windows 7 (partial support, with limited functionality)

Supported Emacs versions:
- Emacs 23.4 and later (including future releases).

---

## 3. Installation

Run the `install_dotemacs.sh` script to install DOT\_EMACS:

### Usage:

```bash
# Install dot_emacs
#  $1 = emacs's binary path
#  $2 = installation target
#  $3 = nosudo
```

### Default Installation:

```bash
~/dot_emacs/install_dotemacs.sh
```

This installs DOT\_EMACS to the default location. Root privileges (via `sudo`) are required.

### Custom Installation:

```bash
~/dot_emacs/install_dotemacs.sh /usr/bin/emacs ~/.emacs.d nosudo
```

This installs DOT\_EMACS to `~/.emacs.d`, bypassing the need for `sudo`.

---

## 4. Default Behavior

DOT\_EMACS:

- Configures Emacs with preinstalled third-party libraries for enhanced functionality.
- Loads user-specific configurations from the ~/.emacs.d/elisp/ directory, including init.el and additional settings if available.

---

## 5. Versioning

DOT\_EMACS uses the `<year>.<month>` versioning format starting from version `11.09`.
Example: `24.12`

---

## 6. Contribution

Contributions are welcome! Here’s how you can help:

1. Fork the repository.
2. Add or improve a feature, or fix an issue.
3. Submit a pull request with clear documentation and changes.

Please ensure your code is well-structured and documented.

---

## 7. License

This repository is dual licensed under the [GPL version 3](https://www.gnu.org/licenses/gpl-3.0.html) or the [LGPL version 3](https://www.gnu.org/licenses/lgpl-3.0.html), at your option.
For full details, please refer to the [LICENSE](doc/LICENSE) file.

Thank you for using and contributing to this repository!
