## OCaml Build Infrastructure

This repository contains the scripts, libraries and command-line tools to
access the opam2 bulk build infrastructure that checks on the health of the
[opam](https://opam.ocaml.org) package manager.

It is still under active development and in preview stage. When it's ready
for significant external contributions, it will migrate to the `ocaml/`
organisation on GitHub.

The main tool you will want to try out is the opam-ci tool, which you can
get by:

```
opam pin add -n obi https://github.com/avsm/obi.git
opam pin add opam-ci https://github.com/avsm/obi.git
```

and then try it out via:

```
opam-ci --help
opam-ci status --help
opam-ci logs --help
```

In the meanwhile, please contact @avsm for more information.
