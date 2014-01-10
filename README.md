Emacs mode for GNU APL
======================

Author contact information
--------------------------

  - Elias Martenson
  - Email: lokedhs@gmail.com

Installing gnu-apl-mode
-----------------------

To install, add the following to your `init.el`:

```lisp
(add-to-list 'load-path "~/path-to/gnu-apl-mode")
(require 'gnu-apl-mode)
```

You can then start the APL interaction using `M-x gnu-apl`.

GNU APL version requirements
----------------------------

This package requires certain features of GNU APL that at the time of
this writing is only available in the latest versions from the
Subversion repository. In partciular, it takes advantage of a command
line option called `--emacs` which enables certain features that are
used by this mode. If your version of GNU APL is too old, you will get
an error message saying that the `--emacs` flag is not understood.

Native code dependencies
------------------------

This version of the mode uses a different method to communicate with
the backend GNU APL instance. Instead of sending commands to the
command line, it now loads native code which allows the Emacs mode to
communicate using a network connection.

GNU APL ships with the native component of this mode and is installed
as `libemacs.so`. If you are using a newer version of this Emacs mode,
it may cause compatibility problems between the native module and the
Emacs Lisp code. If this happens, you need to compile the module
yourself (from the `native` directory) and install it in the `lib/apl`
directory where GNU APL is installed.

Configuring APL font
--------------------

Usually, one wants to use a different font for APL buffers. This can
be done using the following configuration:

```lisp
(defun em-gnu-apl-init ()
  (setq buffer-face-mode-face '(:height 130 :family "APL385 Unicode"))
  (buffer-face-mode))

(add-hook 'gnu-apl-interactive-mode-hook 'em-gnu-apl-init)
(add-hook 'gnu-apl-mode-hook 'em-gnu-apl-init)
```

This enables `buffer-face-mode` with the chosen font when an APL
buffer is opened.

Keymap
------

This mode provides two different ways to input APL characters. The
first is part of the mode itself, and implements the full GNU APL
keymap, when the "super" key is active.

If your keyboard does not have a super key (I highly recommend that
you map one, for example the otherwise useless windows key), you can
also use the `APL-Z` mode. Simply press `C-\` and choose `APL-Z`. This will
provide the same keymap, but prefixed by ".". Pressing dot twice will
output a dot on its own.

Keyboard help
-------------

By default, the keyboard help buffer is opened whenever the GNU APL
interactive mode is started. If you do not want this behaviour, set
the variable `gnu-apl-show-keymap-on-startup` to `nil`. This value can
be customised using `M-x customize-variable`.
