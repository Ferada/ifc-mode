IFC-MODE - Major mode for editing IFC SPF files.

Copyright (C) 2012 Olof-Joachim Frahm <olof@macrolet.net>

Release under a Simplified BSD license.

# INSTALLATION

Run `make` to generate syntax and toc files.  The necessary files are
downloaded and then preprocessed to extract the IFC types.

Then add the following to your `.emacs` file:

    (add-to-list 'load-path "~/path/to/ifc-mode")
    (autoload 'ifc-spf-mode "ifc-mode"
      "Major mode for editing IFC SPF files." t)
    (add-to-list 'auto-mode-alist '("\\.ifc$" . ifc-spf-mode))

(At the moment only IFC2X3 TC1 is used, but in principle all definitions
can be switched rather easily (load another generated file and change
the documentation root URL).  This could also be done automatically by
parsing the correct version from the visited file.)

# USAGE

Syntax highlighting is enabled for the predefined IFC types and
enumerations.  Additionally two functions for following object IDs are
available:  With `ifc-mode-find-tag`, which is bound to `M-.` by
default, the buffer is searched for a numeric tag at the point like
`#42=`.  With `ifc-mode-find-uses`, bound to `M-_` and `M-?`, the buffer
is searched for only `#42`, i.e. all uses of that ID.

`ifc-mode-pop-find-tag`, or the default `pop-tag-mark`, restore the
previous point.  (Wrap-around for `ifc-mode-find-uses` isn't yet
implemented, but would definitely be useful to have.)

Documentation can be found using `ifc-mode-docs-lookup`, bound to
`C-c C-d h`.  The point has to be over an IFC identifier,
e.g. `IFCMATERIAL`, which has to be in the list of defined types.
(Resolving enumeration values isn't implemented yet.)
