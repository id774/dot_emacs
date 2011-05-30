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

setup_dotemacs() {
    test -d $TARGET && $SUDO rm -rf $TARGET/
    test -f $HOME/.emacs && rm -f $HOME/.emacs
    test -d $HOME/.emacs.d && rm -rf $HOME/.emacs.d
    cp $OPTIONS $DOT_EMACS/dot_emacs $HOME/.emacs
    cp $OPTIONS $DOT_EMACS/dot_mew.el $HOME/.mew.el
    test -d $TARGET || $SUDO mkdir -p $TARGET/
    $SUDO cp $OPTIONS $DOT_EMACS/dot_emacs.d/elisp $TARGET/
}

setup_rhtml() {
    if [ -d $GITHUB/rhtml ]; then
        cd $GITHUB/rhtml
        $SUDO git pull
    else
        cd $GITHUB
        $SUDO git clone git://github.com/eschulte/rhtml.git
        cd $GITHUB/rhtml
    fi
}

setup_rinari() {
    if [ -d $GITHUB/rinari ]; then
        cd $GITHUB/rinari
        $SUDO git pull
    else
        cd $GITHUB
        $SUDO git clone git://github.com/eschulte/rinari.git
        cd $GITHUB/rinari
    fi
    $SUDO git submodule init
    $SUDO git submodule update
}

emacs_private_settings() {
    if [ -f $PRIVATE/dot_files/dot_mew.el ]; then
        cp $PRIVATE/dot_files/dot_mew.el $HOME/.mew.el
    fi
    if [ -f $HOME/etc/config.local/dot_mew_el ]; then
        cp $HOME/etc/config.local/dot_mew_el $HOME/.mew.el
    fi
    chmod 600 $HOME/.mew.el

    if [ -f $HOME/etc/config.local/local.el ]; then
        $SUDO cp $OPTIONS $HOME/etc/config.local/*.el $TARGET/elisp/
    fi
    $SUDO vim \
      $HOME/.mew.el \
      $TARGET/elisp/proxy.el \
      $TARGET/elisp/faces.el \
      $TARGET/elisp/emacs-w3m.el \
      $TARGET/elisp/local.el
}

byte_compile_all() {
    cd $TARGET/elisp/3rd-party
    $SUDO $EMACS --batch --eval '(byte-compile-file "js2.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "redo+.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "viewer.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "ruby-block.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "jaspace.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "actionscript-mode.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "fuzzy.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "popup.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "key-chord.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "anything.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "bat-mode.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "git.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "git-blame.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "popwin.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "wb-line-number.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "twitter1-mode.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "twitter2-mode.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "twitter3-mode.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "twitter4-mode.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "twitter5-mode.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "twitter6-mode.el")'
    cd $TARGET/elisp
    $SUDO $EMACS --batch --eval '(byte-compile-file "mew-settings.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "custom.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "delete-empty-file.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "emacs-w3m.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "faces.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "global-set-key.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "jde-config.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "key-chord-define-global.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "kill-all-buffers.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "minor-mode-hack.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "new-file-p.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "persistent-scratch.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "physical-line.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "proxy.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "startup.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "tab4.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "twitter-key.el")'
    $SUDO $EMACS --batch --eval '(byte-compile-file "utils.el")'
}

slink_elisp() {
    test -d $HOME/.emacs.d || mkdir $HOME/.emacs.d
    if [ "$TARGET" != "$HOME/.emacs.d" ]; then
        ln -s $TARGET/elisp $HOME/.emacs.d/elisp
    fi
    test -d $HOME/.emacs.d/anything || mkdir $HOME/.emacs.d/anything
    test -d $HOME/.emacs.d/backups || mkdir $HOME/.emacs.d/backups
    test -d $HOME/.emacs.d/tmp || mkdir $HOME/.emacs.d/tmp
    test -d $HOME/.emacs.d/tramp-auto-save || mkdir $HOME/.emacs.d/tramp-auto-save
    test -d $HOME/.emacs.d/auto-save-list || mkdir $HOME/.emacs.d/auto-save-list
    sudo chmod 750 $HOME/.emacs.d
    sudo chmod 750 $HOME/.emacs.d/anything
    sudo chmod 750 $HOME/.emacs.d/backups
    sudo chmod 750 $HOME/.emacs.d/tmp
    sudo chmod 750 $HOME/.emacs.d/tramp-auto-save
    sudo chmod 750 $HOME/.emacs.d/auto-save-list
}

network_connection() {
    setup_rhtml
    setup_rinari
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
    test -n "$2" || export TARGET=/etc/emacs.d
    #test -n "$2" || export TARGET=$HOME/.emacs.d

    test -n "$3" || SUDO=sudo
    test -n "$3" && SUDO=
    GITHUB=$TARGET/elisp/3rd-party
    DOT_EMACS=$HOME/dot_emacs
}

set_permission() {
    $SUDO chown -R $OWNER $TARGET
}

install_dotemacs() {
    cd
    setup_environment $*
    setup_dotemacs
    ping -c 1 id774.net > /dev/null 2>&1 && network_connection
    emacs_private_settings
    byte_compile_all
    slink_elisp
    test -n "$3" || set_permission
}

install_dotemacs $*
