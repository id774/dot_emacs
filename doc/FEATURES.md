# DOT_EMACS Features

This document is a user-facing reference for the behavior added or changed by
DOT_EMACS.

It is not a complete GNU Emacs key binding reference, and it does not reproduce
all commands or default bindings provided by bundled third-party libraries.
Instead, it documents the bindings, automatic behavior, and defaults that
DOT_EMACS itself installs, enables, or changes.

The Emacs Lisp implementation remains authoritative. Source file names are
included so that each behavior can be traced back to its configuration.

## 1. Key notation

| Notation | Meaning |
| --- | --- |
| `C-` | Control |
| `M-` | Meta |
| `S-` | Shift |
| `RET` | Return / Enter |
| `SPC` | Space |
| `DEL` | Delete |
| `<up>` / `<down>` / `<left>` / `<right>` | Arrow keys |

A sequence such as `C-x C-j` means press `C-x`, then `C-j`.

Key chords are different from ordinary Emacs key sequences. DOT_EMACS enables
`key-chord-mode` with `key-chord-two-keys-delay` set to `0.02` seconds. A chord
such as `jk` therefore means typing the two characters almost simultaneously.

## 2. Global key bindings

### 2.1 Help and Emacs control

| Key | Command | Behavior | Condition | Source |
| --- | --- | --- | --- | --- |
| `C-h` | `delete-backward-char` | Deletes the character before point instead of opening the normal Emacs help prefix. On non-X/non-macOS terminals DOT_EMACS also translates `C-h` to DEL at the keyboard layer. | Always; terminal handling differs by window system | `emacs.d/elisp/global-set-key.el` |
| `C-\` | `help-command` | Opens the Emacs help prefix moved away from `C-h`. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-M-g` | `keyboard-escape-quit` | Cancels the current command or exits a recursive editing state. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-x C-c` | `confirm-save-buffers-kill-emacs` | Asks `quit emacs?` before exiting Emacs. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c 0` / `C-x 7` | `confirm-kill-all-buffers` | Asks for confirmation and then kills all open buffers. | Always | `emacs.d/elisp/global-set-key.el` |
| `M-RET` | `toggle-fullscreen` | Toggles the current frame between normal and full-screen display. | Graphical effect depends on the frame/window system | `emacs.d/elisp/configs.el` |

DOT_EMACS aliases `yes-or-no-p` to `y-or-n-p`, so many confirmation prompts
accept the shorter `y`/`n` answers.

### 2.2 Window navigation

DOT_EMACS treats the arrow keys primarily as window-navigation keys rather than
cursor-movement keys.

| Key | Command | Behavior | Condition | Source |
| --- | --- | --- | --- | --- |
| `<left>` | `windmove-left` | Selects the window to the left. | Always | `emacs.d/elisp/global-set-key.el` |
| `<right>` | `windmove-right` | Selects the window to the right. | Always | `emacs.d/elisp/global-set-key.el` |
| `<up>` | `windmove-up` | Selects the window above. | Always | `emacs.d/elisp/global-set-key.el` |
| `<down>` | `windmove-down` | Selects the window below. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-S-b` | `windmove-left` | Selects the window to the left. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-S-f` | `windmove-right` | Selects the window to the right. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-S-p` | `windmove-up` | Selects the window above. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-S-n` | `windmove-down` | Selects the window below. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-x p` | custom lambda using `other-window` | Selects the previous window rather than the next one. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-w` / `C-c C-c w` / `C-c C-c C-c` / `C-c C-c c` | `other-window` | Selects the next window. | Always | `emacs.d/elisp/global-set-key.el` |
| `M-V` | `scroll-other-window-down` | Scrolls the other window backward. | Always | `emacs.d/elisp/global-set-key.el` |

`windmove-wrap-around` is enabled, so moving past an edge can wrap to a window
on the opposite side. DOT_EMACS also calls `windmove-default-keybindings`,
installing the standard Windmove key set supplied by the running Emacs version.

### 2.3 Window management

| Key | Command | Behavior | Source |
| --- | --- | --- | --- |
| `C-c C-c C-k` | `delete-window` | Deletes the selected window. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c k` | `delete-other-windows` | Keeps the selected window and removes the others. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-y` / `C-c C-c y` | `split-window-vertically` | Splits the current window into upper and lower windows. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-j` | `split-one-window` | Splits left/right only when the frame currently contains one window. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c j` | `split-window-horizontally` | Splits the current window into left and right windows. | `emacs.d/elisp/global-set-key.el` |

### 2.4 Buffer navigation and management

| Key | Command | Behavior | Condition | Source |
| --- | --- | --- | --- | --- |
| `M-n` | `forward-buffer` | Buries the current buffer, effectively moving forward through the buffer list. | Always | `emacs.d/elisp/global-set-key.el` |
| `M-p` | `backward-buffer` | Switches to the buffer at the opposite end of the current buffer list. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c h` | `electric-buffer-list` | Opens the electric buffer list. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-x C-k` | `kill-buffer` | Kills a buffer. This replaces the normal meaning of the `C-x C-k` prefix. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-p` | `revert-current-buffer` | Reloads the current visited file from disk without asking about the current buffer contents. | File-visiting buffers | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c p` | `revert-all-buffers` | Reloads every buffer that visits a file, then returns to the original buffer. | File-visiting buffers | `emacs.d/elisp/global-set-key.el` |
| `C-x b` | `anything` | Uses Anything as the main buffer/file/history selector. | When `anything-config` loads | `emacs.d/elisp/anything-settings.el` |
| `C-x C-b` | `electric-buffer-list` | Provides a minimal buffer-list fallback. | Only when `anything-config` does not load | `emacs.d/elisp/anything-settings.el` |

When Anything is available, its configured sources include buffers, bookmarks,
recent files, file-name history, and `locate` results.

### 2.5 File operations

| Key | Command | Behavior | Source |
| --- | --- | --- | --- |
| `C-x C-w` | `save-buffer` | Saves the current buffer. DOT_EMACS changes this from the traditional `write-file` use. | `emacs.d/elisp/global-set-key.el` |
| `C-x w` | `write-file` | Writes the current buffer to a file, taking over the role moved away from `C-x C-w`. | `emacs.d/elisp/global-set-key.el` |
| `C-M-x C-w` | `save-buffer` | Additional binding for saving the current buffer. | `emacs.d/elisp/global-set-key.el` |
| `C-x C-r` | `find-library` | Opens the Emacs Lisp library that provides a named feature. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-b` | `byte-compile-file` | Byte-compiles an Emacs Lisp file selected by the user. | `emacs.d/elisp/global-set-key.el` |

DOT_EMACS also calls `find-function-setup-keys`, enabling the standard
find-function/find-variable key set supplied by the running Emacs version.

### 2.6 Editing and movement

| Key | Command | Behavior | Condition | Source |
| --- | --- | --- | --- | --- |
| `C-m` | `newline-and-indent` | Inserts a newline and indents the new line. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-j` | `newline` | Inserts a newline without the DOT_EMACS indentation behavior assigned to `C-m`. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-p` | `previous-window-line` | Moves up by a physical/displayed line while preserving the visual column. | Always | `emacs.d/elisp/physical-line.el` |
| `C-n` | `next-window-line` | Moves down by a physical/displayed line while preserving the visual column. | Always | `emacs.d/elisp/physical-line.el` |
| `C-c C-c C-a` / `C-c C-c a` | `beginning-of-buffer` | Moves to the beginning of the buffer. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-e` / `C-c C-c e` | `end-of-buffer` | Moves to the end of the buffer. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-TAB` | `dabbrev-expand` | Expands a dynamic abbreviation. | When the terminal/window system can distinguish the key | `emacs.d/elisp/global-set-key.el` |
| `C-S-TAB` | `dabbrev-completion` | Shows/completes dynamic abbreviation candidates. | When the terminal/window system can distinguish the key | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-u` / `C-c C-c u` | `undo` | Undoes the previous edit. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-r` / `C-c C-c r` | `redo` | Redoes an edit through the `redo` alias. | When undo-tree loads | `emacs.d/elisp/global-set-key.el`, `emacs.d/elisp/redo-settings.el` |
| `M-/` | `redo` | Redoes an edit. This replaces the normal `M-/` dynamic-abbreviation binding. | When undo-tree loads | `emacs.d/elisp/redo-settings.el` |
| `C-c C-c C-z` / `C-c C-c z` | `cua-mode` | Toggles CUA mode; DOT_EMACS uses CUA mainly for rectangle support and disables the normal CUA `C-x`/`C-c`/`C-v` remappings. | Always | `emacs.d/elisp/global-set-key.el`, `emacs.d/elisp/autoloads.el` |
| `C-c C-c -` | `my-decrease-tab-width` | Decreases the current buffer's `tab-width`, down to a minimum of 1. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c +` | `my-increase-tab-width` | Increases the current buffer's `tab-width`. | Always | `emacs.d/elisp/global-set-key.el` |

The additional interactive command `M-x tab4` toggles the current buffer's
`tab-width` between 4 and 8 and rebuilds its tab stops.

### 2.7 Search, replace, and inspection

| Key | Command | Behavior | Source |
| --- | --- | --- | --- |
| `C-c C-c C-q` | `query-replace-regexp` | Interactively replaces matches of a regular expression. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c q` | `query-replace-regexp-eval` | Performs regexp replacement with evaluated replacement expressions. | `emacs.d/elisp/global-set-key.el` |
| `C-x C-q` | `query-replace` | Interactively replaces literal strings. | `emacs.d/elisp/global-set-key.el` |
| `C-x q` | `replace-string` | Replaces literal strings without per-match confirmation. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-g` | `grep-find` | Runs recursive file searching through the grep interface. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c g` | `grep` | Runs grep. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-d` / `C-c C-c d` | `describe-variable` | Describes an Emacs variable. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-f` / `C-c C-c f` | `describe-function` | Describes an Emacs function. | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-b` | `describe-bindings` | Displays active key bindings. | `emacs.d/elisp/global-set-key.el` |
| `M-g M-t` | `google-this` | Searches the current symbol/region using `google-this`; DOT_EMACS uses a Japanese-oriented search URL with a five-year date range. | `google-this` must load | `emacs.d/elisp/google-this-settings.el` |

### 2.8 Display controls

| Key | Command | Behavior | Source |
| --- | --- | --- | --- |
| `C-x t` | `linum-mode` | Toggles line-number display in the current buffer. | `emacs.d/elisp/global-set-key.el` |
| `C-x 5` | `text-scale-increase` | Increases the text scale in the current buffer. | `emacs.d/elisp/global-set-key.el` |
| `C-x 6` | `text-scale-decrease` | Decreases the text scale in the current buffer. | `emacs.d/elisp/global-set-key.el` |
| `C-x C-j` / `C-x j` | `toggle-view-mode` | Switches between the DOT_EMACS read-only/viewing workflow and editable state. | Always | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c C-t` / `C-x 9` | `whitespace-mode` | Toggles whitespace visualization for the current buffer. | Standard `whitespace` library available | `emacs.d/elisp/global-set-key.el`, `emacs.d/elisp/whitespace-settings.el` |
| `C-c C-c t` | `delete-trailing-whitespace` | Deletes trailing spaces and tabs. | Always | `emacs.d/elisp/global-set-key.el` |

### 2.9 Completion, terminal, mail, and browser integration

| Key | Command | Behavior | Condition | Source |
| --- | --- | --- | --- | --- |
| `C-x C-y` / `C-x y` | `auto-complete-mode` | Toggles auto-complete in the current buffer. | `auto-complete` available | `emacs.d/elisp/global-set-key.el` |
| `C-x C-a` / `C-x a` | `multi-term` | Opens a multi-term terminal. | `multi-term` available | `emacs.d/elisp/global-set-key.el` |
| `C-c C-c m` | `mew` | Starts the Mew mail client. | Mew installed and loadable | `emacs.d/elisp/mew-settings.el` |
| `C-x m` | `browse-url-at-point` | Opens the URL at point using emacs-w3m as the configured browser. | emacs-w3m available | `emacs.d/elisp/emacs-w3m.el` |
| `C-c C-c C-l` / `C-c C-c l` | `w3m` | Starts emacs-w3m. | emacs-w3m available | `emacs.d/elisp/emacs-w3m.el` |
| `C-c C-c b` | `anything-git-files` | Selects files from the current Git repository through Anything. | `anything-git-files` available | `emacs.d/elisp/autoloads.el` |

### 2.10 Kill-ring, auto-save, and proxy controls

| Key | Command | Behavior | Source |
| --- | --- | --- | --- |
| `C-c k` / `C-c C-k` | `clear-kill-ring` | Clears the Emacs kill ring and, when GUI selection support is available, clears the system clipboard as well. | `emacs.d/elisp/global-set-key.el`, `emacs.d/elisp/clear-kill-ring.el` |
| `C-x x w` | `auto-save-buffers-enhanced-toggle-activity` | Temporarily enables or disables DOT_EMACS's enhanced direct-to-file automatic saving. | `emacs.d/elisp/auto-save-buffers-settings.el` |
| `C-c M-c p` | `global-proxy-use-toggle` | Toggles the `global-proxy-use` flag used by DOT_EMACS network integration. | `emacs.d/elisp/global-set-key.el` |

`autoloads.el` temporarily binds `C-c k` to `browse-kill-ring` when that package
loads, but `configs.el` is loaded last and then loads `global-set-key.el`.
Therefore the final DOT_EMACS binding for `C-c k` is `clear-kill-ring`.

### 2.11 GNU/Linux graphical input method

| Key | Command | Behavior | Condition | Source |
| --- | --- | --- | --- | --- |
| `<zenkaku-hankaku>` | `toggle-input-method` | Toggles the input method; Mozc is selected as the default input method. | Graphical GNU/Linux and `mozc` available | `emacs.d/elisp/configs.el` |

## 3. Key chords

DOT_EMACS enables `key-chord-mode` when the bundled key-chord library loads.
The two-key recognition delay is `0.02` seconds.

| Chord | Command | Behavior | Source |
| --- | --- | --- | --- |
| `yu` | `auto-complete-mode` | Toggles auto-complete. | `emacs.d/elisp/key-chord-define-global.el` |
| `ty` | `linum-mode` | Toggles line numbers. | `emacs.d/elisp/key-chord-define-global.el` |
| `io` | `windmove-up` | Moves to the window above. | `emacs.d/elisp/key-chord-define-global.el` |
| `,.` | `windmove-down` | Moves to the window below. | `emacs.d/elisp/key-chord-define-global.el` |
| `hj` | `windmove-left` | Moves to the window on the left. | `emacs.d/elisp/key-chord-define-global.el` |
| `l;` | `windmove-right` | Moves to the window on the right. | `emacs.d/elisp/key-chord-define-global.el` |
| `fg` | `keyboard-escape-quit` | Cancels the current command/state. | `emacs.d/elisp/key-chord-define-global.el` |
| `jk` | `toggle-view-mode` | Toggles view/read-only mode. | `emacs.d/elisp/key-chord-define-global.el` |
| `m,` | `forward-buffer` | Moves forward through buffers by burying the current buffer. | `emacs.d/elisp/key-chord-define-global.el` |
| `ui` | `backward-buffer` | Switches toward the opposite end of the buffer list. | `emacs.d/elisp/key-chord-define-global.el` |
| `rt` | `beginning-of-buffer` | Moves to the beginning of the buffer. | `emacs.d/elisp/key-chord-define-global.el` |
| `vb` | `end-of-buffer` | Moves to the end of the buffer. | `emacs.d/elisp/key-chord-define-global.el` |
| `kl` / `nm` | `electric-buffer-list` | Opens the electric buffer list. | `emacs.d/elisp/key-chord-define-global.el` |
| `bn` | `buffer-menu` | Opens the standard buffer menu. | `emacs.d/elisp/key-chord-define-global.el` |
| `df` | `find-file` | Opens a file. | `emacs.d/elisp/key-chord-define-global.el` |
| `as` / `;:` | `anything` | Opens Anything. | `emacs.d/elisp/key-chord-define-global.el` |
| `sd` | `anything-find-files` | Opens Anything's file selector. | `emacs.d/elisp/key-chord-define-global.el` |
| `we` | `cua-mode` | Toggles CUA mode/rectangle support. | `emacs.d/elisp/key-chord-define-global.el` |
| `zx` | `undo` | Undoes the previous edit. | `emacs.d/elisp/key-chord-define-global.el` |
| `qw` | `redo` | Redoes an edit through undo-tree. | `emacs.d/elisp/key-chord-define-global.el` |

## 4. Mode-specific key bindings

### 4.1 View mode

DOT_EMACS turns `view-mode` into a pager-like navigation environment. It also
keeps view mode active when switching buffers and sets `view-read-only` to true.

| Key | Command | Behavior |
| --- | --- | --- |
| `h` | `backward-word` | Moves backward one word. |
| `l` | `forward-word` | Moves forward one word. |
| `j` | `next-line` | Moves to the next logical line. |
| `k` | `previous-line` | Moves to the previous logical line. |
| `J` | `next-window-line` | Moves down one physical/displayed line. |
| `K` | `previous-window-line` | Moves up one physical/displayed line. |
| `b` | `scroll-down` | Scrolls backward by a screenful. |
| `f` / `SPC` | `scroll-up` | Scrolls forward by a screenful. |
| `w` | `forward-word` | Moves forward one word. |
| `e` | `backward-word` | Moves backward one word. |
| `n` | custom one-line scroll | Scrolls forward by one line. |
| `p` | custom one-line scroll | Scrolls backward by one line. |
| `[` | `forward-sexp` | Moves forward across one balanced expression. |
| `]` | `backward-sexp` | Moves backward across one balanced expression. |
| `c` | `scroll-other-window-down` | Scrolls the other window backward. |
| `v` | `scroll-other-window` | Scrolls the other window forward. |

Source: `emacs.d/elisp/view-mode-key.el`

In view mode, `RET` is further specialized for two major modes:

| Major mode | `RET` command | Behavior |
| --- | --- | --- |
| C mode | `gtags-find-tag-from-here` | Jumps to the GNU GLOBAL tag at point. |
| Emacs Lisp mode | `find-function-at-point` | Opens the function definition at point. |

### 4.2 Dired

| Key | Command | Behavior |
| --- | --- | --- |
| `RET` | `dired-find-alternate-file` | Enters the selected file/directory without accumulating another Dired buffer. |
| `a` | `dired-find-file` | Opens the selected file/directory in the normal Dired way. |
| `<backspace>` / `DEL` | `dired-up-directory` | Moves to the parent directory. |
| `<left>` | `windmove-left` | Selects the window on the left. |
| `<right>` | `windmove-right` | Selects the window on the right. |
| `<up>` | `windmove-up` | Selects the window above. |
| `<down>` | `windmove-down` | Selects the window below. |
| `r` | `wdired-change-to-wdired-mode` | Makes the Dired listing directly editable. |

The `r` binding is installed only when the built-in/available `wdired` library
can be required.

Sources: `emacs.d/elisp/dired-settings.el`,
`emacs.d/elisp/autoloads.el`

### 4.3 Anything

When `anything-config` loads, DOT_EMACS configures Anything as a combined
selector for buffers, bookmarks, recent files, file-name history, and `locate`.

| Key | Command | Behavior |
| --- | --- | --- |
| `C-p` | `anything-previous-line` | Selects the previous candidate. |
| `C-n` | `anything-next-line` | Selects the next candidate. |
| `C-v` | `anything-next-source` | Moves to the next Anything source. |
| `M-v` | `anything-previous-source` | Moves to the previous Anything source. |

Source: `emacs.d/elisp/anything-settings.el`

### 4.4 Term / multi-term

In `term-raw-map`, DOT_EMACS sends familiar Emacs movement keys to the shell
while reserving other keys for terminal/buffer management.

| Key | Command | Behavior |
| --- | --- | --- |
| `C-p` | `term-send-previous-line` | Sends `C-p` to the terminal process. |
| `C-n` | `term-send-next-line` | Sends `C-n` to the terminal process. |
| `C-b` | `term-send-backward-char` | Sends `C-b` to the terminal process. |
| `C-f` | `term-send-forward-char` | Sends `C-f` to the terminal process. |
| `C-h` | `term-send-backspace` | Sends backspace to the terminal process. |
| `C-y` | `term-paste` | Pastes into the terminal. |
| `ESC ESC` | `term-send-raw` | Sends the next input raw to the terminal. |
| `C-S-p` | `multi-term-prev` | Switches to the previous multi-term buffer. |
| `C-S-n` | `multi-term-next` | Switches to the next multi-term buffer. |
| `<up>` / `<down>` / `<left>` / `<right>` | `windmove-*` | Moves between Emacs windows instead of sending arrow keys to the terminal. |

The terminal foreground/background defaults are green on black.

Source: `emacs.d/elisp/multi-term-settings.el`

### 4.5 Python mode

| Key | Command | Behavior | Condition |
| --- | --- | --- | --- |
| `C-c p` | `python-pep8` | Runs the configured PEP 8 checker. | Only when `python-pep8` is defined |
| `C-c F` | `py-autopep8` | Formats the current Python buffer with autopep8. | `py-autopep8` must load |
| `C-c f` | `py-autopep8-region` | Formats the active region with autopep8. | `py-autopep8` must load |

Source: `emacs.d/elisp/python-mode-settings.el`

### 4.6 GNU GLOBAL / gtags

When gtags support is available, DOT_EMACS enables `gtags-mode` automatically
for C-derived modes and installs:

| Key | Command | Behavior |
| --- | --- | --- |
| `M-f` | `gtags-find-tag` | Finds a tag definition. |
| `M-r` | `gtags-find-rtag` | Finds references/reverse tags. |
| `M-s` | `gtags-find-symbol` | Finds a symbol. |
| `C-t` | `gtags-pop-stack` | Returns through the gtags navigation stack. |

Source: `emacs.d/elisp/lang-mode.el`

### 4.7 Minibuffer completion on Emacs 23.4 through 29.x

GNU Emacs 30 and later uses built-in visible minibuffer completion instead.
On older supported Emacs versions DOT_EMACS loads zlc and changes minibuffer
arrow keys:

| Key | Command | Behavior |
| --- | --- | --- |
| `<down>` | `zlc-select-next-vertical` | Selects the candidate below. |
| `<up>` | `zlc-select-previous-vertical` | Selects the candidate above. |
| `<right>` | `zlc-select-next` | Selects the next candidate. |
| `<left>` | `zlc-select-previous` | Selects the previous candidate. |

Source: `emacs.d/elisp/zlc-settings.el`

### 4.8 Legacy bindings on Emacs earlier than 25

These bindings are deliberately limited to GNU Emacs 23.4 and 24.x.

When `sequential-command-config` is available:

| Key | Command | Behavior |
| --- | --- | --- |
| `C-a` | `seq-home` | Repeated presses alternate between indentation and line start. |
| `C-e` | `seq-end` | Repeated presses alternate between line end positions. |
| `M-u` | `seq-upcase-backward-word` | Sequential backward-word uppercasing. |
| `M-c` | `seq-capitalize-backward-word` | Sequential backward-word capitalization. |
| `M-l` | `seq-downcase-backward-word` | Sequential backward-word lowercasing. |

Org mode receives corresponding sequential `C-a` and `C-e` commands.

When `smartchr` is available, the literal keys `{`, `>`, and `F` cycle through
predefined insertion patterns.

Source: `emacs.d/elisp/smartchr-settings.el`

## 5. Automatic behavior

### 5.1 Startup and user interface

DOT_EMACS changes the default Emacs presentation at startup:

- hides the startup/splash message;
- disables cursor blinking;
- hides the menu bar;
- hides the tool bar and scroll bar in graphical frames;
- disables xterm mouse mode;
- displays the current time;
- shows line and column numbers in the mode line;
- enables `which-func-mode` so the current function can appear in the mode line;
- enables matching-parenthesis highlighting;
- highlights the current line;
- hides the cursor in non-selected windows;
- marks empty lines;
- enables transient region highlighting;
- enables automatic image-file display;
- enables automatic decompression of compressed files;
- suppresses the bell by setting `ring-bell-function` to `ignore`;
- suppresses automatic native-compilation warning pop-up windows while keeping
  the warnings in `*Warnings*`;
- wraps text at the window edge, including partially-width windows;
- sets graphical frame transparency to 90% active / 75% inactive;
- limits `*Messages*` history to 200 entries.

Sources: `emacs.d/elisp/init.el`, `emacs.d/elisp/configs.el`

### 5.2 Fonts, colors, coding, and graphical frames

DOT_EMACS selects the Japanese language environment and prefers UTF-8 Unix
coding.

In graphical frames it uses a green-on-black color scheme with a red cursor and
custom syntax-highlighting colors. Font-lock uses maximum decoration without a
file-size limit.

Platform-specific defaults include:

- GNU/Linux: DejaVu Sans Mono for ASCII, IPAexGothic for Japanese, and a
  150x50 default frame;
- macOS: Menlo for ASCII, Hiragino Kaku Gothic ProN W3 for Japanese, a
  230x65 default frame, and italics disabled for the main italic/comment/string
  faces;
- Windows: VL Gothic is configured as the expected font.

On macOS, Command is mapped to Meta and Option/Alt to Super.

Source: `emacs.d/elisp/faces.el`

### 5.3 Opening files and view mode

Opening a file triggers DOT_EMACS-specific behavior:

- `find-file-hook` enables `view-mode` when it is not already active;
- line-number display is enabled for visited files;
- non-writable existing files are routed through `view-file`;
- `view-read-only` is enabled, integrating read-only files with view mode;
- a root-owned, non-writable file causes DOT_EMACS to ask whether to reopen it
  through TRAMP's sudo method;
- TRAMP file buffers are renamed with the remote method as a prefix, for
  example `sudo:...`.

The `C-x C-j` / `C-x j` binding is the intended way to move between the
view/read-only workflow and editing.

Sources: `emacs.d/elisp/configs.el`,
`emacs.d/elisp/view-mode-key.el`

### 5.4 Direct automatic saving

DOT_EMACS disables Emacs's ordinary `auto-save-default` behavior and instead
enables `auto-save-buffers-enhanced`.

This is important: the enhanced mode does not merely create a crash-recovery
auto-save file. After 15 seconds of idle time it scans buffers and directly
calls `save-buffer` for modified, writable, non-read-only file buffers whose
paths match its include rules.

The default DOT_EMACS configuration includes all file paths and defines no
exclusion regexps, so ordinary writable file buffers are eligible.

Use `C-x x w` to toggle this activity at runtime.

Sources: `emacs.d/elisp/configs.el`,
`emacs.d/elisp/auto-save-buffers-settings.el`,
`emacs.d/elisp/3rd-party/auto-save-buffers-enhanced.el`

### 5.5 Backup files

Although native auto-save is disabled, normal Emacs backup files remain enabled.

DOT_EMACS:

- stores backups under `~/.emacs.d/backups`;
- uses `backup-by-copying`, preserving the original file's inode instead of
  renaming the original file into the backup;
- requires a final newline when saving files.

Source: `emacs.d/elisp/configs.el`

### 5.6 Empty-file deletion prompt

After saving a file-visiting buffer whose contents are completely empty,
DOT_EMACS asks:

`Delete file and kill buffer?`

If confirmed, the file is deleted from disk and the buffer is killed.

Source: `emacs.d/elisp/delete-empty-file.el`

### 5.7 History and kill-ring persistence

DOT_EMACS persists minibuffer history and the kill ring with two mechanisms:

- built-in `savehist-mode` is enabled, saves every 300 seconds, and explicitly
  includes `kill-ring`;
- bundled `savekill` writes the kill ring to
  `~/.emacs.d/kill-ring-saved.el` every time `kill-new` changes it, and reloads
  that file after initialization.

This gives the kill ring both periodic `savehist` persistence and immediate
per-update persistence.

Sources: `emacs.d/elisp/autoloads.el`,
`emacs.d/elisp/3rd-party/savekill.el`

### 5.8 Recent files

`recentf-ext` enables `recentf-mode` and DOT_EMACS configures it to:

- store its data in `~/.emacs.d/.recentf`;
- retain up to 9,999 entries;
- exclude the `.recentf` file itself;
- disable automatic recent-file cleanup;
- save the recent-file list every 3,600 seconds of idle time;
- treat displayed file buffers as recently used;
- include Dired directories in recent history.

Sources: `emacs.d/elisp/recentf-ext-settings.el`,
`emacs.d/elisp/3rd-party/recentf-ext.el`

### 5.9 Scratch buffer lifetime

`persistent-scratch.el` does **not** persist the contents of `*scratch*` across
Emacs sessions.

Its actual behavior is to prevent the `*scratch*` buffer from disappearing:

- attempts to kill `*scratch*` recreate/clear it and reject the kill;
- after any save, DOT_EMACS recreates `*scratch*` if it is unexpectedly absent.

Source: `emacs.d/elisp/persistent-scratch.el`

### 5.10 Asynchronous Emacs Lisp byte compilation

When the bundled asynchronous compiler loads, every Emacs Lisp buffer enables
`auto-async-byte-compile-mode`. Saving an eligible `.el` file starts a separate
batch Emacs process to byte-compile it.

DOT_EMACS deliberately excludes:

- files under `/junk/`;
- `init.el`;
- `autoloads.el`;
- `configs.el`;
- `lang-mode.el`;
- `screen.el`;
- `diminish-settings.el`;
- `anything-settings.el`.

These exclusions match the repository's selective byte-compilation policy.

Sources: `emacs.d/elisp/auto-async-settings.el`,
`emacs.d/elisp/3rd-party/auto-async-byte-compile.el`

### 5.11 Completion

When the bundled `auto-complete` library loads, DOT_EMACS enables
`global-auto-complete-mode`.

Ruby buffers use case-sensitive auto-complete matching.

Minibuffer completion is version-dependent:

- GNU Emacs 30+: `minibuffer-visible-completions` is enabled;
- GNU Emacs 23.4 through 29.x: the historical zlc completion UI is loaded.

Sources: `emacs.d/elisp/auto-complete-settings.el`,
`emacs.d/elisp/autoloads.el`,
`emacs.d/elisp/zlc-settings.el`

### 5.12 Fuzzy indentation detection

DOT_EMACS enables `global-fuzzy-format-mode` and sets the default indentation
choice to spaces.

For supported programming modes, fuzzy-format examines existing lines and
selects `indent-tabs-mode` according to whether tabs or spaces dominate. The
mode-line buffer identification is annotated with `[T]` or `[S]`.

The library's automatic whole-buffer reformatting and automatic indentation
options remain disabled by default, so enabling global fuzzy-format does not by
itself tabify/untabify or reindent the entire buffer.

Sources: `emacs.d/elisp/autoloads.el`,
`emacs.d/elisp/3rd-party/fuzzy-format.el`

### 5.13 Whitespace visualization

DOT_EMACS enables `global-whitespace-mode`.

It marks:

- full-width Japanese spaces;
- tabs;
- trailing whitespace;
- newline/end-of-line positions.

Ordinary ASCII spaces are deliberately not marked.

Source: `emacs.d/elisp/whitespace-settings.el`

### 5.14 Lisp editing assistance

When Paredit is available, it is enabled automatically in:

- Emacs Lisp mode;
- Lisp Interaction mode;
- Lisp mode;
- IELM mode.

Eldoc is also enabled automatically in Emacs Lisp, Lisp Interaction, and IELM
buffers with a 0.2-second idle delay.

Sources: `emacs.d/elisp/paredit-settings.el`,
`emacs.d/elisp/configs.el`

### 5.15 Undo and redo

When undo-tree loads, DOT_EMACS enables `global-undo-tree-mode`, defines
`redo` as `undo-tree-redo`, and binds it to `M-/` in addition to the
DOT_EMACS undo/redo bindings.

Source: `emacs.d/elisp/redo-settings.el`

### 5.16 Dired behavior

In addition to the Dired-specific keys listed earlier, DOT_EMACS changes Dired
defaults:

- when two Dired windows are visible, the other Dired directory is preferred as
  the destination for copy/move operations (`dired-dwim-target`);
- recursive directory copies are allowed without asking for recursive-copy
  policy each time;
- incremental search in Dired is limited to file names;
- `dired-find-alternate-file` is enabled;
- `dired-async-mode` is enabled when the bundled async Dired integration loads.

Sources: `emacs.d/elisp/dired-settings.el`,
`emacs.d/elisp/autoloads.el`

### 5.17 Popup-window management

When popwin is available, DOT_EMACS replaces `display-buffer-function` with
popwin's display function and uses a popup height of 40% of the frame.

Special handling is configured for:

- `*anything*`;
- Dired buffers;
- VC-related buffers;
- `*git-*` buffers.

Source: `emacs.d/elisp/popwin-el.el`

### 5.18 Buffer naming

When `uniquify` loads, duplicate buffer names use
`post-forward-angle-brackets` style, adding distinguishing path components after
the base buffer name.

Source: `emacs.d/elisp/autoloads.el`

### 5.19 CUA rectangle support

DOT_EMACS globally enables `cua-mode`, but sets `cua-enable-cua-keys` to `nil`.
This keeps rectangle-editing support without replacing the traditional Emacs
`C-x`, `C-c`, and `C-v` command families with CUA cut/copy/paste bindings.

Source: `emacs.d/elisp/autoloads.el`

### 5.20 Optional startup enhancements

When their libraries are available, DOT_EMACS also enables or initializes:

| Feature | DOT_EMACS behavior | Source |
| --- | --- | --- |
| `sense-region` | Calls `sense-region-on`. | `emacs.d/elisp/autoloads.el` |
| `mic-paren` | Calls `paren-activate`. | `emacs.d/elisp/autoloads.el` |
| `develock` | Enables global font-lock through the package. | `emacs.d/elisp/autoloads.el` |
| `highlight-unique-symbol` | Enables unique-symbol highlighting. | `emacs.d/elisp/autoloads.el` |
| `dired-async` | Enables asynchronous Dired operations. | `emacs.d/elisp/autoloads.el` |
| `minibuf-isearch` | Loads minibuffer incremental-search extensions. | `emacs.d/elisp/autoloads.el` |
| `git` / `git-blame` | Loads the bundled/available Git integration. | `emacs.d/elisp/autoloads.el` |
| `open-junk-file` | Loads the available scratch/junk-file command package. | `emacs.d/elisp/autoloads.el` |
| `timidity` | Registers the TiMidity++ command for autoloading. | `emacs.d/elisp/autoloads.el` |
| `italk` | Loads it quietly when available. | `emacs.d/elisp/autoloads.el` |

## 6. Editing defaults

DOT_EMACS changes several editing defaults globally:

| Setting | DOT_EMACS behavior | Source |
| --- | --- | --- |
| Tabs vs spaces | Uses spaces by default (`indent-tabs-mode` is nil). | `emacs.d/elisp/configs.el` |
| Tab width | Defaults to 4 columns. | `emacs.d/elisp/configs.el` |
| Final newline | Requires a final newline when saving. | `emacs.d/elisp/configs.el` |
| Fill column | Defaults to 79. | `emacs.d/elisp/configs.el` |
| `C-k` at line start | `kill-whole-line` is enabled, so killing from the beginning of a line includes the newline. | `emacs.d/elisp/configs.el` |
| Narrowing | `narrow-to-region` and `narrow-to-page` are enabled commands. | `emacs.d/elisp/configs.el` |
| Search highlighting | Search and query-replace highlighting are enabled with no lazy-highlight startup delay. | `emacs.d/elisp/configs.el` |
| Scrolling | `scroll-conservatively` is 1 for line-oriented scrolling. | `emacs.d/elisp/configs.el` |
| End-of-buffer movement | `next-line` does not create new lines at end of buffer. | `emacs.d/elisp/configs.el` |

For C-derived modes DOT_EMACS selects the Stroustrup style, enables hungry
deletion, uses spaces, and sets `c-basic-offset` to 4.

A separate Ruby style derived from BSD is also registered with two-column case,
label, and statement-case indentation.

Source: `emacs.d/elisp/configs.el`

## 7. Language-specific behavior

### 7.1 Ruby

When Ruby support is available:

- `.rb` files and `ruby` interpreter scripts use `ruby-mode`;
- `inf-ruby` keys are installed in Ruby buffers;
- auto-complete matching is case-sensitive.

On GNU/Linux, DOT_EMACS additionally attempts to enable:

- `ruby-electric-mode` for automatic matching delimiters;
- `ruby-block-mode` with matching-block highlighting;
- the `rubydb` debugger autoload.

Sources: `emacs.d/elisp/ruby-optional-load.el`,
`emacs.d/elisp/auto-complete-settings.el`

### 7.2 Python

DOT_EMACS loads Python formatting/checking integration.

If `/opt/python/current/bin` exists, it is added to `exec-path`. If
`/opt/python/current/bin/python` is executable, it becomes
`python-shell-interpreter`; otherwise DOT_EMACS leaves Emacs's normal Python
interpreter selection unchanged.

Source: `emacs.d/elisp/python-mode-settings.el`

### 7.3 C and related modes

C-derived modes:

- use the Stroustrup style;
- use four-space indentation;
- disable indentation tabs;
- enable hungry deletion;
- automatically enable gtags mode when GNU GLOBAL support is available.

Source: `emacs.d/elisp/configs.el`,
`emacs.d/elisp/lang-mode.el`

### 7.4 Text mode

All `text-mode` buffers are enlarged by one text-scale step automatically.

Source: `emacs.d/elisp/lang-mode.el`

### 7.5 Haskell

When Haskell mode is available, DOT_EMACS associates `.hs`, `.hi`, `.gs`,
`.lhs`, and `.lgs` files with it, and enables Haskell documentation and
indentation helpers.

Source: `emacs.d/elisp/lang-mode.el`

### 7.6 Rails / HTML templates

When available:

- `.erb` and `.rhtml` use `rhtml-mode`;
- entering `rhtml-mode` launches Rinari integration.

Source: `emacs.d/elisp/lang-mode.el`

### 7.7 JavaScript, stylesheets, and templates

When the corresponding modes are available:

- `.js` uses `js2-mode` with `js2-basic-offset` set to 4;
- `.scss` uses `scss-mode`, with compile-on-save disabled;
- `.sass` uses `sass-mode`, with compile-on-save disabled;
- `.haml` uses `haml-mode` with spaces for indentation;
- `.coffee` and `Cakefile` use `coffee-mode`;
- `.styl` uses `sws-mode`.

Source: `emacs.d/elisp/lang-mode.el`

### 7.8 Other file associations

DOT_EMACS adds or conditionally adds mode associations for:

- `.rd` -> `rd-mode`;
- `.php` and `php` interpreter scripts -> `php-mode`;
- `.as` -> `actionscript-mode`;
- `.pig` -> `pig-latin-mode`;
- `.bat`, `.cmd`, `CONFIG.*`, and `AUTOEXEC.*` -> `bat-mode`;
- `.md` and `.txt` -> `markdown-mode`, when available;
- `.erl` -> `erlang-mode` only on Emacs earlier than 27.

Source: `emacs.d/elisp/lang-mode.el`

### 7.9 YaTeX / LaTeX

`.tex` files use YaTeX. DOT_EMACS sets:

- the LaTeX command to `/usr/local/sbin/platex2pdf`;
- the viewer to `evince` on GNU/Linux;
- the viewer to macOS Preview on macOS;
- automatic filling off inside YaTeX buffers.

Source: `emacs.d/elisp/yatex-mode.el`

## 8. Remote files and external commands

### 8.1 TRAMP

When TRAMP loads, DOT_EMACS sets:

- default remote method: `scpx`;
- remote shell prompt detection: `^.*[#$%>] *`;
- TRAMP auto-save directory: `~/.emacs.d/tramp-auto-save`;
- TRAMP verbosity: 3;
- TRAMP debug buffer support on.

Source: `emacs.d/elisp/tramp-settings.el`

### 8.2 Executable search path

At startup DOT_EMACS prepends `/usr/local/bin` and `/opt/local/bin` to the
Emacs executable search path and process `PATH`.

If `/opt/homebrew/bin` exists, it is also prepended. This covers the standard
Apple Silicon Homebrew location without imposing it on systems where it does
not exist.

Source: `emacs.d/elisp/init.el`

### 8.3 Temporary files on GNU/Linux

On GNU/Linux, when `/dev/shm` exists, DOT_EMACS uses it as
`temporary-file-directory`. Other platforms retain the normal Emacs temporary
directory.

Source: `emacs.d/elisp/configs.el`

### 8.4 Terminal title updates

When Emacs runs without a graphical window under an xterm- or screen-like
`TERM`, DOT_EMACS updates the terminal title/hard status when the selected
buffer changes.

Source: `emacs.d/elisp/screen.el`

## 9. Version-specific behavior

DOT_EMACS supports GNU Emacs 23.4 and later, but several user-visible behaviors
depend on the running version.

| Version | Behavior |
| --- | --- |
| Emacs 23.4-24.x | Legacy sequential `C-a`/`C-e`, backward case-conversion helpers, and smartchr insertion bindings can be enabled. JSP `autostart` is also loaded on Emacs earlier than 25. |
| Emacs 23.4-29.x | zlc provides visible minibuffer completion and arrow-key candidate navigation. |
| Emacs earlier than 27 | Erlang mode is loaded/associated when available. |
| Emacs 28+ | Abbreviations are saved silently in `~/.emacs.d/abbrev_defs`; `abbrev-mode` itself remains off by default. |
| Emacs 30+ | DOT_EMACS uses built-in `minibuffer-visible-completions` instead of zlc. |

Native-compilation warning pop-ups are suppressed when the corresponding
Emacs 28+ variables exist, while warnings remain available in the warnings
buffer.

Sources: `emacs.d/elisp/init.el`,
`emacs.d/elisp/autoloads.el`,
`emacs.d/elisp/configs.el`,
`emacs.d/elisp/lang-mode.el`,
`emacs.d/elisp/smartchr-settings.el`,
`emacs.d/elisp/zlc-settings.el`

## 10. Platform-specific behavior

| Platform/environment | DOT_EMACS behavior | Source |
| --- | --- | --- |
| GNU/Linux graphical Emacs | Uses Linux font/frame defaults; enables Mozc and `<zenkaku-hankaku>` when Mozc is available. | `emacs.d/elisp/faces.el`, `emacs.d/elisp/configs.el` |
| GNU/Linux with `/dev/shm` | Uses `/dev/shm` for temporary files. | `emacs.d/elisp/configs.el` |
| GNU/Linux Ruby editing | Attempts to enable ruby-electric, ruby-block, and Ruby debugger integration. | `emacs.d/elisp/ruby-optional-load.el` |
| macOS | Uses Menlo/Hiragino fonts, maps Command to Meta and Option to Super, disables selected italic faces, and uses Preview for YaTeX output. | `emacs.d/elisp/faces.el`, `emacs.d/elisp/yatex-mode.el` |
| Apple Silicon/Homebrew | Adds `/opt/homebrew/bin` to executable paths when present. | `emacs.d/elisp/init.el` |
| X Window System | Binds the Delete key to `delete-char`; `C-h` is not keyboard-translated. | `emacs.d/elisp/global-set-key.el` |
| macOS/NS window system | Leaves `C-h` untranslated, then binds it as backward delete. | `emacs.d/elisp/global-set-key.el` |
| Other/terminal window systems | Translates `C-h` to DEL before keymap processing. | `emacs.d/elisp/global-set-key.el` |
| Terminal xterm/screen | Updates the terminal title when buffer selection changes. | `emacs.d/elisp/screen.el` |

## 11. Source reference

The main user-visible behavior documented above is defined by these
project-owned configuration files:

- `emacs.d/elisp/init.el` - startup paths, platform executable paths, native-compilation warning behavior.
- `emacs.d/elisp/autoloads.el` - module loading, optional feature activation, completion-version split, history persistence.
- `emacs.d/elisp/configs.el` - core defaults, file-opening behavior, UI, editing defaults, fullscreen, sudo/TRAMP integration.
- `emacs.d/elisp/global-set-key.el` - main global key map.
- `emacs.d/elisp/key-chord-define-global.el` - global key chords.
- `emacs.d/elisp/view-mode-key.el` - pager-like view-mode behavior.
- `emacs.d/elisp/physical-line.el` - physical-line `C-p`/`C-n` movement.
- `emacs.d/elisp/dired-settings.el` - Dired navigation and defaults.
- `emacs.d/elisp/anything-settings.el` - Anything sources and key map.
- `emacs.d/elisp/multi-term-settings.el` - terminal appearance and term key map.
- `emacs.d/elisp/python-mode-settings.el` - Python checking and formatting.
- `emacs.d/elisp/lang-mode.el` - language-mode associations, gtags, and per-language hooks.
- `emacs.d/elisp/ruby-optional-load.el` - Ruby integration.
- `emacs.d/elisp/yatex-mode.el` - YaTeX integration.
- `emacs.d/elisp/emacs-w3m.el` - browser integration.
- `emacs.d/elisp/mew-settings.el` - Mew integration.
- `emacs.d/elisp/google-this-settings.el` - Google search integration.
- `emacs.d/elisp/redo-settings.el` - undo-tree and redo.
- `emacs.d/elisp/auto-complete-settings.el` - global auto-complete.
- `emacs.d/elisp/auto-save-buffers-settings.el` - enhanced automatic saving.
- `emacs.d/elisp/auto-async-settings.el` - asynchronous Emacs Lisp byte compilation.
- `emacs.d/elisp/recentf-ext-settings.el` - recent-file persistence.
- `emacs.d/elisp/whitespace-settings.el` - whitespace visualization.
- `emacs.d/elisp/paredit-settings.el` - Lisp structural editing.
- `emacs.d/elisp/tramp-settings.el` - remote-file defaults.
- `emacs.d/elisp/persistent-scratch.el` - scratch-buffer lifetime.
- `emacs.d/elisp/delete-empty-file.el` - empty-file deletion prompt.
- `emacs.d/elisp/clear-kill-ring.el` - kill-ring and clipboard clearing.
- `emacs.d/elisp/diminish-settings.el` - compact mode-line names.
- `emacs.d/elisp/popwin-el.el` - popup-window routing.
- `emacs.d/elisp/screen.el` - terminal title updates.
- `emacs.d/elisp/faces.el` - language environment, fonts, colors, and frame defaults.
- `emacs.d/elisp/smartchr-settings.el` - Emacs <25 legacy sequential/smartchr bindings.
- `emacs.d/elisp/zlc-settings.el` - Emacs <=29 minibuffer completion navigation.
- `emacs.d/elisp/tab4.el` - interactive 4/8-column tab-width toggle.

Bundled third-party libraries may provide additional commands and their own
default key bindings. Those are intentionally outside the scope of this file
unless DOT_EMACS explicitly enables, changes, or binds them.
