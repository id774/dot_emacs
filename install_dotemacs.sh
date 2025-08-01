#!/bin/sh

########################################################################
# install_dotemacs.sh: Install dot_emacs Configuration
#
#  Description:
#  This script installs the dot_emacs configuration files to the specified
#  target directory. It compiles Emacs Lisp scripts, sets the appropriate
#  permissions, and optionally removes existing configurations.
#
#  Author: id774 (More info: http://id774.net)
#  Source Code: https://github.com/id774/dot_emacs
#  License: The GPL version 3, or LGPL version 3 (Dual License).
#  Contact: idnanashi@gmail.com
#
#  Usage:
#      ./install_dotemacs.sh [emacs_binary] [target_path] [nosudo]
#      ./install_dotemacs.sh --uninstall [emacs_binary] [target_path] [nosudo]
#
#  Examples:
#      ./install_dotemacs.sh                    # Install using default emacs
#      ./install_dotemacs.sh /usr/bin/emacs     # Specify system emacs
#      ./install_dotemacs.sh /Applications/Emacs.app/Contents/MacOS/Emacs
#          # macOS: Use Emacs.app binary directly
#
#  Options:
#      -h, --help        Show this help message and exit.
#      -u, --uninstall   Remove all installed dot_emacs components.
#
#  Notes:
#  - [emacs_binary]: Path to the Emacs binary (default: emacs).
#    macOS users can use: /Applications/Emacs.app/Contents/MacOS/Emacs
#  - [target_path]: Path to the installation directory (default: /usr/local/etc/emacs.d).
#  - [nosudo]: If specified, the script runs without sudo.
#  - The script will remove existing Emacs configurations before installation.
#  - Byte-compilation is performed to improve Emacs performance.
#  - The --uninstall option will remove installed files and user configuration.
#
#  Version History:
#  v3.0 2025-08-01
#       Add --uninstall option to cleanly remove installed components.
#  v2.3 2025-06-23
#       Unified usage output to display full script header and support common help/version options.
#  v2.2 2025-04-27
#       Add strict error checking for all critical filesystem operations
#       and unify log output with [INFO] and [ERROR] tags.
#  v2.1 2025-03-22
#       Unify usage information by extracting help text from header comments.
#  v2.0 2025-03-17
#       Standardized documentation format and added system checks.
#  [Further version history truncated for brevity]
#  v1.0 2009-05-18
#       Initial release.
#
########################################################################

# Display full script header information extracted from the top comment block
usage() {
    awk '
        BEGIN { in_header = 0 }
        /^#{10,}$/ { if (!in_header) { in_header = 1; next } else exit }
        in_header && /^# ?/ { print substr($0, 3) }
    ' "$0"
    exit 0
}

# Check if required commands are available and executable
check_commands() {
    for cmd in "$@"; do
        cmd_path=$(command -v "$cmd" 2>/dev/null)
        if [ -z "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif [ ! -x "$cmd_path" ]; then
            echo "[ERROR] Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "[ERROR] This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Install dot_emacs files to the target directory
setup_dotemacs() {
    echo "[INFO] Setting up dot_emacs configuration..."

    [ -d "$TARGET" ] && $SUDO rm -rf "$TARGET/"
    [ -f "$HOME/.emacs" ] && rm -f "$HOME/.emacs"
    [ -d "$HOME/.emacs.d" ] && rm -rf "$HOME/.emacs.d"

    if ! cp $OPTIONS "$SCRIPT_HOME/dot_emacs" "$HOME/.emacs"; then
        echo "[ERROR] Failed to copy dot_emacs to $HOME/.emacs." >&2
        exit 1
    fi

    if ! cp $OPTIONS "$SCRIPT_HOME/dot_mew.el" "$HOME/.mew.el"; then
        echo "[ERROR] Failed to copy dot_mew.el to $HOME/.mew.el." >&2
        exit 1
    fi

    [ -d "$TARGET" ] || $SUDO mkdir -p "$TARGET/" || {
        echo "[ERROR] Failed to create target directory $TARGET." >&2
        exit 1
    }

    if ! $SUDO cp $OPTIONS "$SCRIPT_HOME/emacs.d/elisp" "$TARGET/"; then
        echo "[ERROR] Failed to copy elisp directory to $TARGET/." >&2
        exit 1
    fi
}

# Apply user-specific settings for Emacs
emacs_private_settings() {
    echo "[INFO] Applying private Emacs settings..."

    [ -f "$HOME/etc/config.local/dot_mew.el" ] && cp $OPTIONS "$HOME/etc/config.local/dot_mew.el" "$HOME/.mew.el"

    chmod 600 "$HOME/.mew.el"

    for file in emacs-w3m.el proxy.el faces.el; do
        if [ -f "$HOME/etc/config.local/$file" ]; then
            if ! $SUDO cp $OPTIONS "$HOME/etc/config.local/$file" "$TARGET/elisp/"; then
                echo "[ERROR] Failed to copy $file to $TARGET/elisp/." >&2
                exit 1
            fi
        fi
    done
}

# Compile an Emacs Lisp file
emacs_batch_byte_compile() {
    while [ $# -gt 0 ]; do
        $SUDO "$EMACS" --batch -Q -f batch-byte-compile "$1"
        shift
    done
}

# Byte-compile all necessary Emacs Lisp files
byte_compile_all() {
    echo "[INFO] Byte-compiling Emacs Lisp files..."

    cd "$TARGET/elisp/3rd-party/helm" && $SUDO make

    cd "$TARGET/elisp/3rd-party/jade-mode" && emacs_batch_byte_compile \
        sws-mode.el \
        jade-mode.el

    cd "$TARGET/elisp/3rd-party/ruby-mode" && emacs_batch_byte_compile \
        inf-ruby.el \
        ruby-mode.el \
        rdoc-mode.el \
        ruby-style.el \
        ruby-electric.el \
        rubydb2x.el \
        rubydb3x.el \
        ruby-additional.el

    cd "$TARGET/elisp/3rd-party" && emacs_batch_byte_compile \
        py-autopep8.el \
        browse-kill-ring.el \
        cl-lib.el \
        js2.el \
        undo-tree.el \
        shadow.el \
        viewer.el \
        ruby-block.el \
        jaspace.el \
        auto-async-byte-compile.el \
        auto-save-buffers-enhanced.el \
        actionscript-mode.el \
        flymake-cursor.el \
        fuzzy.el \
        popup.el \
        key-chord.el \
        anything.el \
        bat-mode.el \
        erlang.el \
        findr.el \
        inflections.el \
        git.el \
        git-blame.el \
        google-this.el \
        minibuf-isearch.el \
        minor-mode-hack.el \
        multi-term.el \
        open-junk-file.el \
        paredit.el \
        popwin.el \
        scss-mode.el \
        haml-mode.el \
        sass-mode.el \
        savekill.el \
        smartchr.el \
        sequential-command.el \
        recentf-ext.el \
        web-mode.el \
        wb-line-number.el \
        zencoding-mode.el

    cd "$TARGET/elisp" && emacs_batch_byte_compile \
        init.el \
        mew-settings.el \
        clear-kill-ring.el \
        delete-empty-file.el \
        dired-settings.el \
        emacs-w3m.el \
        faces.el \
        global-set-key.el \
        key-chord-define-global.el \
        kill-all-buffers.el \
        new-file-p.el \
        persistent-scratch.el \
        physical-line.el \
        proxy.el \
        tab4.el \
        utils.el \
        yatex-mode.el
}

# Create symbolic links for Emacs configuration files
slink_elisp() {
    echo "[INFO] Creating symlinks for Emacs configuration..."

    if [ ! -d "$HOME/.emacs.d" ]; then
        echo "[INFO] Creating directory: $HOME/.emacs.d"
        if ! mkdir "$HOME/.emacs.d"; then
            echo "[ERROR] Failed to create $HOME/.emacs.d." >&2
            exit 1
        fi
    fi

    if [ "$TARGET" != "$HOME/.emacs.d" ]; then
        echo "[INFO] Creating symlink: $HOME/.emacs.d/elisp -> $TARGET/elisp"
        if ! ln -fs "$TARGET/elisp" "$HOME/.emacs.d/elisp"; then
            echo "[ERROR] Failed to create symlink for elisp" >&2
            exit 1
        fi

        if [ -L "$TARGET/elisp/elisp" ]; then
            echo "[INFO] Removing redundant symlink: $TARGET/elisp/elisp"
            if ! $SUDO rm -f "$TARGET/elisp/elisp"; then
                echo "[ERROR] Failed to remove redundant symlink $TARGET/elisp/elisp." >&2
                exit 1
            fi
        fi
    fi

    for dir in site-lisp anything backups tmp tramp-auto-save auto-save-list; do
        if [ ! -d "$HOME/.emacs.d/$dir" ]; then
            echo "[INFO] Creating directory: $HOME/.emacs.d/$dir."
            if ! mkdir "$HOME/.emacs.d/$dir"; then
                echo "[ERROR] Failed to create $HOME/.emacs.d/$dir." >&2
                exit 1
            fi
        fi
        if ! $SUDO chmod 750 "$HOME/.emacs.d/$dir"; then
            echo "[ERROR] Failed to set permission for $HOME/.emacs.d/$dir." >&2
            exit 1
        fi
    done

    echo "[INFO] Creating adaptive history file: $HOME/.emacs.d/anything/anything-c-adaptive-history"
    if ! touch "$HOME/.emacs.d/anything/anything-c-adaptive-history"; then
        echo "[ERROR] Failed to create anything-c-adaptive-history file." >&2
        exit 1
    fi

    echo "[INFO] Symlink setup for Emacs configuration completed successfully."
}

# Initialize environment variables
setup_environment() {
    echo "[INFO] Starting environment setup..."

    case "$(uname)" in
        Darwin) OPTIONS=-Rv; OWNER=root:wheel ;;
        *) OPTIONS=-Rvd; OWNER=root:root ;;
    esac

    EMACS=${1:-emacs}
    check_commands "$EMACS"

    TARGET=${2:-/usr/local/etc/emacs.d}
    if [ -n "$3" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi
    echo "[INFO] Using sudo: ${SUDO:-no}"

    if [ "$SUDO" = "sudo" ]; then
        check_sudo
    else
        OWNER="$(id -un):$(id -gn)"
    fi
    echo "[INFO] Copy options: $OPTIONS, Owner: $OWNER"

    export SCRIPT_HOME=$(dirname "$(realpath "$0" 2>/dev/null || readlink -f "$0")")

    echo "[INFO] Environment setup completed."
}

# Set file permissions
set_permission() {
    echo "[INFO] Setting ownership for $TARGET"
    if ! $SUDO chown -R "$OWNER" "$TARGET"; then
        echo "[ERROR] Failed to change ownership for $TARGET." >&2
        exit 1
    fi

    if ! $SUDO chown "$(id -un):$(id -gn)" "$TARGET/elisp/3rd-party/nxhtml/etc/schema/xhtml-loader.rnc"; then
        echo "[ERROR] Failed to change ownership for xhtml-loader.rnc." >&2
        exit 1
    fi

    if ! $SUDO chown -R "$(id -un):$(id -gn)" "$TARGET/elisp/3rd-party/ruby-mode/"; then
        echo "[ERROR] Failed to change ownership for ruby-mode directory." >&2
        exit 1
    fi
}

# Uninstall dot_emacs configuration
uninstall() {
    shift
    check_commands sudo rm rmdir
    setup_environment "$@"

    echo "[INFO] Uninstalling dot_emacs configuration..."

    [ -f "$HOME/.emacs" ] && rm -f "$HOME/.emacs"
    [ -f "$HOME/.mew.el" ] && rm -f "$HOME/.mew.el"
    [ -L "$HOME/.emacs.d/elisp" ] && rm -f "$HOME/.emacs.d/elisp"

    for dir in site-lisp anything backups tmp tramp-auto-save auto-save-list; do
        [ -d "$HOME/.emacs.d/$dir" ] && rm -rf "$HOME/.emacs.d/$dir"
    done

    [ -f "$HOME/.emacs.d/anything/anything-c-adaptive-history" ] && \
        rm -f "$HOME/.emacs.d/anything/anything-c-adaptive-history"

    [ -d "$HOME/.emacs.d" ] && rmdir "$HOME/.emacs.d" 2>/dev/null

    if [ -d "$TARGET" ]; then
        echo "[INFO] Removing installed target directory: $TARGET"
        if ! $SUDO rm -rf "$TARGET"; then
            echo "[ERROR] Failed to remove target directory $TARGET." >&2
            exit 1
        fi
    fi

    echo "[INFO] dot_emacs uninstallation completed successfully."
}

# Perform installation steps
install() {
    cd || exit 1

    check_commands sudo cp mkdir chmod chown ln rm id dirname uname
    setup_environment "$@"
    setup_dotemacs
    emacs_private_settings
    byte_compile_all
    slink_elisp
    [ -n "$3" ] || set_permission

    echo "[INFO] Installation completed successfully."
}

# Main entry point of the script
main() {
    case "$1" in
        -h|--help|-v|--version)
            usage
            ;;
        -u|--uninstall)
            uninstall "$@"
            ;;
        ""|*)
            install "$@"
            ;;
    esac
    return 0
}

# Execute main function
main "$@"
exit $?
