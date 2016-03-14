Emacs mode for GNU APL
======================

Author contact information
--------------------------

  - Elias Mårtenson
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

This package requires certain features of GNU APL that was implemented
as of version 1.2. In partciular, it takes advantage of a command line
option called `--emacs` which enables certain features that are used
by this mode. If your version of GNU APL is too old, you will get an
error message saying that the `--emacs` flag is not understood.

Native code dependencies
------------------------

This application contains a native part that is compiled into a shared
library that is loaded by GNU APL. The source code for this library
can be found in the directory `native`.

GNU APL ships with this library, but if you are using a newer version
of this Emacs mode, it may cause compatibility problems between the
native module and the Emacs Lisp code. If this happens, you need to
compile the module yourself and install it in the `lib/apl` directory
where GNU APL is installed.

Keymap
------

This mode provides two different ways to input APL characters. The
first is part of the mode itself, and implements the full GNU APL
keymap, when the "super" key is active.

If your keyboard does not have a super key (I highly recommend that
you map one, for example the otherwise useless windows key), you can
also use the `APL-Z` mode. Simply press `C-\` and choose `APL-Z`. This
will provide the same keymap, but prefixed by ".". Pressing dot twice
will output a dot on its own.

Keyboard help
-------------

By default, the keyboard help buffer is opened whenever the GNU APL
interactive mode is started. If you do not want this behaviour, set
the variable `gnu-apl-show-keymap-on-startup` to `nil`. This value can
be customised using `M-x customize-variable`.

Fonts
-----

Some operating systems (in particular, Fedora) does not ship with
fonts that contain all the nescessary APL symbols. One free font that
contains all symbols and also looks good is GNU Free Mono. It can be
downloaded here: https://www.gnu.org/software/freefont/

Configuring APL font
--------------------

Usually, one wants to use a different font for APL buffers. This mode
includes a face called `gnu-apl-default` which is used in various
places, such as the help buffers. However, it's not currently enabled
by default in the interactive session, nor in APL buffers.

If you want to enable this, add the following to your
`~/.emacs.d/init.el`:

```lisp
(defun em-gnu-apl-init ()
  (setq buffer-face-mode-face 'gnu-apl-default)
  (buffer-face-mode))

(add-hook 'gnu-apl-interactive-mode-hook 'em-gnu-apl-init)
(add-hook 'gnu-apl-mode-hook 'em-gnu-apl-init)
```

This enables `buffer-face-mode` with the chosen font when an APL
buffer is opened.

This may be changed to be the default in a future version.
