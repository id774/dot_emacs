#!/bin/sh
#
########################################################################
# Install dot_emacs
#  $1 = emacs's binary path
#  $2 = installation target
#  $3 = nosudo
#
#  Maintainer: id774 <idnanashi@gmail.com>
#
# v1.20 2025-03-05
#       Added sudo privilege check when --sudo option is specified.
# v1.19 5/22,2014
#       Clean up obsolete code.
# v1.18 2/4,2014
#       Explicit specification sudo.
# v1.17 11/27,2013
#       Add ruby-additional.el.
# v1.16 3/15,2013
#       Replace redo+.el to undo-tree.el for redo.
# v1.15 1/18,2013
#       Delete unnecessary symlinks.
# v1.14 1/8,2013
#       Don't use the editor.
# v1.13 2/12,2012
#       Change default install target to /usr/local/etc/emacs.d.
# v1.12 2/6,2012
#       Improvement permission for the problem of first start.
# v1.11 12/8,2011
#       Add auto-install.
# v1.10 9/13,2011
#       Ruby Mode, and rename directory name.
#  v1.9 5/24,2011
#       Selectable installation target.
#  v1.8 3/29,2011
#       Add mew.
#  v1.7 3/18,2011
#       Remove CEDET.
#  v1.6 10/4,2010
#       Refactoring.
#  v1.5 8/9,2010
#       Generate account setting files.
#  v1.4 3/9,2010
#       Auto generate twitter elisp.
#  v1.3 3/7,2010
#       Refactoring.
#  v1.2 3/2,2010
#       Byte compile by /usr/bin/emacs on mac default.
#  v1.1 2/21,2010
#       Specify emacs path.
#  v1.0 5/18,2009
#       Stable.
########################################################################

# Check if the user has sudo privileges (password may be required)
check_sudo() {
    if ! sudo -v 2>/dev/null; then
        echo "Error: This script requires sudo privileges. Please run as a user with sudo access."
        exit 1
    fi
}

setup_dotemacs() {
    test -d $TARGET && $SUDO rm -rf $TARGET/
    test -f $HOME/.emacs && rm -f $HOME/.emacs
    test -d $HOME/.emacs.d && rm -rf $HOME/.emacs.d
    cp $OPTIONS $DOT_EMACS/dot_emacs $HOME/.emacs
    cp $OPTIONS $DOT_EMACS/dot_mew.el $HOME/.mew.el
    test -d $TARGET || $SUDO mkdir -p $TARGET/
    $SUDO cp $OPTIONS $DOT_EMACS/emacs.d/elisp $TARGET/
}

emacs_private_settings() {
    if [ -f $PRIVATE/dot_files/dot_mew.el ]; then
        cp $PRIVATE/dot_files/dot_mew.el $HOME/.mew.el
    fi
    if [ -f $HOME/etc/config.local/dot_mew.el ]; then
        cp $HOME/etc/config.local/dot_mew.el $HOME/.mew.el
    fi
    chmod 600 $HOME/.mew.el

    if [ -f $HOME/etc/config.local/proxy.el ]; then
        $SUDO cp $OPTIONS $HOME/etc/config.local/*.el $TARGET/elisp/
    fi
}

emacs_batch_byte_compile() {
    while [ $# -gt 0 ]
    do
        $SUDO $EMACS --batch -Q -f batch-byte-compile $1
        shift
    done
}

byte_compile_all() {
    cd $TARGET/elisp/3rd-party/helm
    $SUDO make
    cd $TARGET/elisp/3rd-party/jade-mode
    emacs_batch_byte_compile \
      sws-mode.el \
      jade-mode.el
    cd $TARGET/elisp/3rd-party/ruby-mode
    emacs_batch_byte_compile \
      inf-ruby.el \
      ruby-mode.el \
      rdoc-mode.el \
      ruby-style.el \
      ruby-electric.el \
      rubydb2x.el \
      rubydb3x.el \
      ruby-additional.el
    cd $TARGET/elisp/3rd-party
    emacs_batch_byte_compile \
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
    cd $TARGET/elisp
    emacs_batch_byte_compile \
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

slink_elisp() {
    test -d $HOME/.emacs.d || mkdir $HOME/.emacs.d
    if [ "$TARGET" != "$HOME/.emacs.d" ]; then
        ln -fs $TARGET/elisp $HOME/.emacs.d/elisp
        test -L $TARGET/elisp/elisp && $SUDO rm $TARGET/elisp/elisp
    fi
    test -d $HOME/.emacs.d/site-lisp || mkdir $HOME/.emacs.d/site-lisp
    test -d $HOME/.emacs.d/anything || mkdir $HOME/.emacs.d/anything
    test -d $HOME/.emacs.d/backups || mkdir $HOME/.emacs.d/backups
    test -d $HOME/.emacs.d/tmp || mkdir $HOME/.emacs.d/tmp
    test -d $HOME/.emacs.d/tramp-auto-save || mkdir $HOME/.emacs.d/tramp-auto-save
    test -d $HOME/.emacs.d/auto-save-list || mkdir $HOME/.emacs.d/auto-save-list
    $SUDO chmod 750 $HOME/.emacs.d
    $SUDO chmod 750 $HOME/.emacs.d/site-lisp
    $SUDO chmod 750 $HOME/.emacs.d/anything
    $SUDO chmod 750 $HOME/.emacs.d/backups
    $SUDO chmod 750 $HOME/.emacs.d/tmp
    $SUDO chmod 750 $HOME/.emacs.d/tramp-auto-save
    $SUDO chmod 750 $HOME/.emacs.d/auto-save-list
    touch $HOME/.emacs.d/anything/anything-c-adaptive-history
}

setup_environment() {
    SCRIPTS=$HOME/scripts
    PRIVATE=$HOME/private/scripts

    case $OSTYPE in
      *darwin*)
        OPTIONS=-Rv
        OWNER=root:wheel
        test -n "$1" || EMACS=/usr/bin/emacs
        ;;
      *)
        OPTIONS=-Rvd
        OWNER=root:root
        test -n "$1" || EMACS=emacs
        ;;
    esac

    test -n "$1" && EMACS=$1
    TARGET=$HOME/.emacs.d
    test -n "$2" && export TARGET=$2
    test -n "$2" || export TARGET=/usr/local/etc/emacs.d
    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    test "$3" = "sudo" && SUDO=sudo

    # Check sudo privileges if sudo is required
    if [ "$SUDO" = "sudo" ]; then
        check_sudo
    fi

    GITHUB=$TARGET/elisp/3rd-party
    DOT_EMACS=$HOME/dot_emacs
}

set_permission() {
    $SUDO chown -R $OWNER $TARGET
    $SUDO chown $USER $TARGET/elisp/3rd-party/nxhtml/etc/schema/xhtml-loader.rnc
    $SUDO chown -R $USER $TARGET/elisp/3rd-party/ruby-mode/
}

install_dotemacs() {
    cd
    setup_environment $*
    setup_dotemacs
    emacs_private_settings
    byte_compile_all
    slink_elisp
    test -n "$3" || set_permission
}

install_dotemacs $*
