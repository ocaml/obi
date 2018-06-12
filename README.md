## OCaml Build Infrastructure

This repository contains the scripts, libraries and command-line tools to
access the opam2 bulk build infrastructure that checks on the health of the
[opam](https://opam.ocaml.org) package manager.

The main tool you will want to try out is the opam-ci tool, which you can
get by:

```
opam pin add -n obi https://github.com/ocaml/obi.git
opam pin add opam-ci https://github.com/ocaml/obi.git
```

and then try it out via:

```
opam-ci --help
opam-ci status --help
opam-ci logs --help
```

In the meanwhile, please contact @avsm for more information.
