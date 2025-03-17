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
#  License: LGPLv3 (Details: https://www.gnu.org/licenses/lgpl-3.0.html)
#  Contact: idnanashi@gmail.com
#
#  Version History:
#  v2.0 2025-03-17
#       Standardized documentation format and added system checks.
#  [Further version history truncated for brevity]
#  v1.0 2009-05-18
#       Initial release.
#
#  Usage:
#  ./install_dotemacs.sh [emacs_binary] [target_path] [nosudo]
#
#  Notes:
#  - [emacs_binary]: Path to the Emacs binary (default: emacs).
#  - [target_path]: Path to the installation directory (default: /usr/local/etc/emacs.d).
#  - [nosudo]: If specified, the script runs without sudo.
#  - The script will remove existing Emacs configurations before installation.
#  - Byte-compilation is performed to improve Emacs performance.
#
########################################################################

# Display help message
show_help() {
    cat <<EOF
Usage: $(basename "$0") [emacs_binary] [target_path] [nosudo]

Options:
  -h, --help    Show this help message and exit.

Description:
  This script installs the dot_emacs configuration files to the specified
  target directory, compiles Emacs Lisp scripts, sets appropriate permissions,
  and optionally removes existing configurations.
EOF
}

# Function to check required commands
check_commands() {
    for cmd in "$@"; do
        if ! command -v "$cmd" >/dev/null 2>&1; then
            echo "Error: Command '$cmd' is not installed. Please install $cmd and try again." >&2
            exit 127
        elif ! [ -x "$(command -v "$cmd")" ]; then
            echo "Error: Command '$cmd' is not executable. Please check the permissions." >&2
            exit 126
        fi
    done
}

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access." >&2
        exit 1
    fi
}

# Install dot_emacs files to the target directory
setup_dotemacs() {
    [ -d "$TARGET" ] && $SUDO rm -rf "$TARGET/"
    [ -f "$HOME/.emacs" ] && rm -f "$HOME/.emacs"
    [ -d "$HOME/.emacs.d" ] && rm -rf "$HOME/.emacs.d"

    cp $OPTIONS "$SCRIPT_HOME/dot_emacs" "$HOME/.emacs"
    cp $OPTIONS "$SCRIPT_HOME/dot_mew.el" "$HOME/.mew.el"
    [ -d "$TARGET" ] || $SUDO mkdir -p "$TARGET/"
    $SUDO cp $OPTIONS "$SCRIPT_HOME/emacs.d/elisp" "$TARGET/"
}

# Apply user-specific settings for Emacs
emacs_private_settings() {
    [ -f "$HOME/etc/config.local/dot_mew.el" ] && cp "$HOME/etc/config.local/dot_mew.el" "$HOME/.mew.el"

    chmod 600 "$HOME/.mew.el"

    if [ -f "$HOME/etc/config.local/proxy.el" ]; then
        $SUDO cp $OPTIONS "$HOME/etc/config.local/proxy.el" "$TARGET/elisp/"
    fi

    if [ -f "$HOME/etc/config.local/faces.el" ]; then
        $SUDO cp $OPTIONS "$HOME/etc/config.local/faces.el" "$TARGET/elisp/"
    fi
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
    [ -d "$HOME/.emacs.d" ] || mkdir "$HOME/.emacs.d"

    if [ "$TARGET" != "$HOME/.emacs.d" ]; then
        ln -fs "$TARGET/elisp" "$HOME/.emacs.d/elisp"
        [ -L "$TARGET/elisp/elisp" ] && $SUDO rm "$TARGET/elisp/elisp"
    fi

    for dir in site-lisp anything backups tmp tramp-auto-save auto-save-list; do
        [ -d "$HOME/.emacs.d/$dir" ] || mkdir "$HOME/.emacs.d/$dir"
        $SUDO chmod 750 "$HOME/.emacs.d/$dir"
    done

    touch "$HOME/.emacs.d/anything/anything-c-adaptive-history"
}

# Initialize environment variables
setup_environment() {
    case "$OSTYPE" in
        *darwin*)
            OPTIONS=-Rv
            OWNER=root:wheel
            ;;
        *)
            OPTIONS=-Rvd
            OWNER=root:root
            ;;
    esac

    EMACS=${1:-emacs}
    check_commands "$EMACS"

    TARGET=${2:-/usr/local/etc/emacs.d}
    if [ -n "$3" ]; then
        SUDO=""
    else
        SUDO="sudo"
    fi
    echo "Using sudo: ${SUDO:-no}"

    if [ "$SUDO" = "sudo" ]; then
        check_sudo
    else
        OWNER="$(id -un):$(id -gn)"
    fi
    echo "Copy options: $OPTIONS, Owner: $OWNER"

    export SCRIPT_HOME=$(dirname "$(realpath "$0" 2>/dev/null || readlink -f "$0")")
}

# Set file permissions
set_permission() {
    $SUDO chown -R "$OWNER" "$TARGET"
    $SUDO chown "$(id -un):$(id -gn)" "$TARGET/elisp/3rd-party/nxhtml/etc/schema/xhtml-loader.rnc"
    $SUDO chown -R "$(id -un):$(id -gn)" "$TARGET/elisp/3rd-party/ruby-mode/"
}

# Main function to execute the script
main() {
    # Parse command-line arguments
    for arg in "$@"; do
        case "$arg" in
            -h|--help)
                show_help
                exit 0
                ;;
        esac
    done

    cd | exit 1

    check_commands sudo cp mkdir chmod chown ln id dirname
    setup_environment "$@"
    setup_dotemacs
    emacs_private_settings
    byte_compile_all
    slink_elisp
    [ -n "$3" ] || set_permission
}

# Execute main function
main "$@"
