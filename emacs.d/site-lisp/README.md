# User-Local Emacs Customization

This directory is reserved for user-specific Emacs Lisp configuration. It allows
users to extend or override DOT_EMACS without changing its managed files.

Local settings may include package configuration, key bindings, editing
preferences, fonts, frame dimensions, and operating-system-specific behavior.
The files in this repository directory are not personal settings installed
automatically into a user's home directory.

## 1. Files and Loading

User configuration belongs under `~/.emacs.d/site-lisp/`. Only `loader.el` is
loaded automatically by DOT_EMACS; other files in that directory must be loaded
explicitly. For example, `loader.el` may contain:

```elisp
(load "my-settings")
(load "my-keybindings")
```

These file names are illustrative, not required names. The directory is in
`load-path`, but being in `load-path` does not cause a file to be evaluated.

The bundled initialization sequence loads `faces.el` and `configs.el` before
the optional user-local `loader.el`. Keep overrides in `site-lisp` rather than
editing the bundled configuration.

## 2. Configuration Timing and Scope

A setting loaded after bundled configuration can still be affected by later
startup processing. Also distinguish between a setting applied to the current
frame or buffer and a default used for instances created later.

Choose an initialization hook when the intended behavior depends on another
component or startup phase already being initialized. Do not add a hook merely
because a customization is local.

## 3. Operating Systems and Display Environments

Use `system-type` for operating-system-specific settings, such as `gnu/linux`
or `darwin`. Use `display-graphic-p` when a setting requires a graphical frame.
An operating-system check alone does not distinguish graphical from terminal
Emacs sessions.

Desktop environments, graphical backends, font availability, display scaling,
and window managers can affect visual settings. Do not assume that a setting
verified on one desktop environment behaves identically on another.

## 4. Frames and Fonts

`initial-frame-alist` provides parameters for the initial frame, while
`default-frame-alist` provides defaults for frames created later. Updating
these variables does not necessarily modify an already existing frame.

Frame width and height are normally expressed in character columns and lines.
Their rendered pixel dimensions depend on the font and display environment.
Large requested dimensions may be constrained by the window manager or the
available screen area.

A font applied while loading initialization files can be affected by later
frame initialization. In a tested GNU/Linux setup, manual reloading applied a
font customization that did not take effect at startup under Xfce; applying the
font from `window-setup-hook` resolved that case. The same customization worked
at startup under GNOME. This observation does not identify a specific Xfce
component as the cause, nor establish the same problem on macOS.

`window-setup-hook` runs after the initial frame setup. If startup timing is
confirmed to be the problem, a graphical font can be applied at that stage:

```elisp
(add-hook 'window-setup-hook
          (lambda ()
            (when (display-graphic-p)
              (set-frame-font "Monospace-12"))))
```

This is an illustrative font, not a recommended system-wide value. The hook
handles initial startup, not every subsequently created frame. Use appropriate
frame defaults when later frames also need customization. Loading a file after
startup does not rerun `window-setup-hook` automatically.

Use a font installed in the local environment. In terminal sessions, font
selection is normally controlled by the terminal emulator rather than the
Emacs graphical-frame settings.

## 5. Packages and Key Bindings

Keep user-installed package configuration in local files. For packages managed
by `package.el`, allow that package manager to handle `~/.emacs.d/elpa/`;
do not add the `elpa` directory itself to `load-path`.

Load a package before using its configuration, and use mode hooks for settings
that require a particular mode to be initialized. Local key bindings may
override bundled bindings, but a mode-specific keymap may not be available
until its corresponding mode has been loaded.

## 6. Installation and Preservation

The installer manages the bundled configuration and its `elisp` tree. It
preserves existing user-local content under `~/.emacs.d/site-lisp/` during
install, reinstall, update, and uninstall. It does not copy example
customization files from this repository directory into the user's home.

The installed `~/.emacs` is managed by DOT_EMACS and may be replaced by the
installer. Keep personal configuration in `site-lisp`, not in that entry point.

## 7. Troubleshooting

When a local setting does not take effect:

1. Confirm that `loader.el` explicitly loads the intended file. Use
   `(locate-library "my-settings")` to check whether an example library name
   resolves; locating a file alone does not prove that it was evaluated.
2. Compare behavior at startup with manually evaluating the local file. If
   manual evaluation works, investigate startup timing and later overrides.
3. For graphical frame settings, inspect `(frame-parameter nil 'font)`,
   `(face-attribute 'default :height)`, `(frame-width)`, and `(frame-height)`.
   Compare with `initial-frame-alist` and `default-frame-alist` as applicable.
4. Check the Emacs version, startup method, graphical versus terminal mode,
   available fonts, display scaling, and desktop/session configuration.

Keep troubleshooting changes local until the responsible behavior is established.
Do not change the bundled DOT_EMACS configuration merely to work around an
unverified environment-specific difference.

For repository-wide compatibility and maintenance rules, see
[Emacs Lisp Design Guidelines](../../doc/GUIDELINES).
