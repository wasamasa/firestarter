firestarter
===========

.. image:: https://raw.githubusercontent.com/wasamasa/firestarter/master/img/firestarter.gif

About
-----

``firestarter`` lets you execute all kinds of things after saving a
buffer.  You can run shell commands, execute interactive Emacs
commands and evaluate arbitrary pieces of Emacs Lisp.  Support for
shell commands includes asynchronous process control, format chars in
command strings and report of process on common process termination
states.

Installation
------------

Install from `Marmalade <https://marmalade-repo.org/>`_ or `MELPA
(Stable) <http://melpa.org/>`_ with ``M-x package-install RET
firestarter RET``.

Usage
-----

Enable it interactively with ``M-x firestarter-mode`` or by adding the
following to your init file:

.. code:: cl

    (firestarter-mode)

You will want to customize the ``firestarter`` variable for anything
to happen on save.  This can be done in many ways, for instance by
using ``M-: (setq firestarter value)`` interactively for
experimentation.  To set up a default, insert the following in your
init file:

.. code:: elisp

    (setq-default firestarter value)

Use a hook for limiting the default to certain major modes:

.. code:: elisp

    (define my-customize-firestarter ()
      (setq firestarter value))
    (add-hook 'my-mode-hook 'my-customize-firestarter)

Project-specific values belong to a directory-local variable.  ``M-x
add-dir-local-variable`` is useful to create or extend the appropriate
file interactively.  The resulting ``.dir-locals.el`` file should look
like this:

.. code:: elisp

    ((nil . ((firestarter . value))))


File-specific values are set up with a file-local variable.  Using
``M-x add-file-local-variable`` will result in the following footer:

.. code:: elisp

    ;; Local Variables:
    ;; firestarter: value
    ;; End:

If you prefer the shorter variant using the header, ``M-x
add-file-local-variable-prop-line`` will result in:

.. code:: elisp

    ;; -*- firestarter: value; -*-

To reapply directory-local and file-local variables, either use ``M-x
normal-mode`` (doesn't reload file from disk), ``M-x revert-buffer``
(reloads file from disk), entering the corresponding major mode of
your file or changing major modes.

----

To run the command interactively (instead on the next save), use ``M-x
firestarter``.  If a command has become stuck, you can either
terminate it outside of Emacs (with a command like ``pkill`` or
``htop``) or inside Emacs with ``M-x firestarter-abort`` for the
current buffer.

In case you dislike confirming file-local variables manually or
whitelisting them on a case-by-case basis and are willing to take the
security risks, you can whitelist all instances of ``firestarter`` by
putting the following in your init file:

.. code:: elisp

    (put 'firestarter 'safe-local-variable 'identity)

Further Customization
---------------------

The ``firestarter`` variable can take a multitude of values:

====== =========================
Type   Usage
====== =========================
Symbol Interactive command
List   Arbitrary Emacs Lisp code
String Shell command
====== =========================

The symbol and list type are evaluated with ``call-interactively`` and
``eval`` and do not offer any further options.  It's possible to have
greater control over the string type by using the list type and
``firestarter-command`` which accepts the command and an optional
reporting type as argument.

The string type has a few extra features, one of them being format
code support.  Use the following as file-local variable to convert
this document into a HTML file on each save:

.. code:: rst

    .. -*- firestarter: "rst2html %f > %s.html" -*-

The following format codes (see the ``firestarter-format`` docstring)
are supported:

===== ===============
Code  Interpretation
===== ===============
%b    Buffer name
%p    File path
%d    Directory name
%f    File name
%s    File stem
%e    File extension
%%    Percentage sign
===== ===============

The other supported feature of the shell command type is reporting of
the shell command output.  Reporting is disabled by default,
customizing ``firestarter-type`` in the same manner as described
previously for the ``firestarter`` variable will display the reporting
buffer (see ``firestarter-buffer-name``) if a certain condition is met
by the shell command return code:

============ =================================
Value        Meaning
============ =================================
nil, 'silent Don't report at all
'success     Report if return code is zero
'failure     Report if return code is not zero
t, 'finished Report after any return code
============ =================================

Usage examples
--------------

All examples given are in the form of file-local variables as headers.

Run ``checkdoc`` on an Emacs Lisp library to check for stylistic blunders:

.. code:: elisp

    ;; -*- firestarter: checkdoc -*-

Execute ERT tests interactively:

.. code:: elisp

    ;; -*- firestarter: ert-run-tests-interactively -*-

Use ``M-x compile`` with ``make``:

.. code:: scheme

    ;; -*- firestarter: (compile "make") -*-

Restart a Rails application using Phusion Passenger:

.. code:: ruby

    # -*- firestarter: (shell-command "touch tmp/restart.txt")

Run ``tup upd`` in the current directory:

.. code:: c

    // -*- firestarter: "tup upd"; firestarter-type: failure -*-

Deploy code with ``rsync``:

.. code:: python

    # -*- firestarter: "rsync -avz -e ssh /src host:/dest" -*-

Contributing
------------

If you find bugs, have suggestions or any other problems, feel free to
report an issue on the issue tracker or hit me up on IRC, I'm always on
``#emacs``.  Patches are welcome, too, just fork, work on a separate
branch and open a pull request with it.

Alternatives
------------

I wrote this package because none of the following alternatives
convinced me:

- `hookify <https://github.com/Silex/hookify>`_ resembles the Lisp
  type for interactive usage only
- `auto-shell-command
  <https://github.com/ongaeshi/auto-shell-command>`_ implements the
  shell command type with init.el usage only
- `watch-buffer <https://github.com/mjsteger/watch-buffer>`_
  implements all types, but requires interactive usage
- `recompile-on-save <https://github.com/maio/recompile-on-save.el>`_
  surely does something, but doesn't even have a proper README
- ``M-x compile`` is shell-command only and pretty weird, but at least
  looks pretty
- `auto-recompile <https://github.com/tuhdo/auto-recompile>`_ does
  away with the most glaring problem of ``M-x compile``, not being
  re-run on save, but shares its other issues
- `previewing-mode <https://github.com/crowding/previewing-mode>`_
  targets a similiar problem, but has lots of overlap with this mode,
  perhaps it's got ideas worth stealing.
