# OCaml Build Infrastructure

This repository contains the scripts, libraries and command-line tools to
access the opam2 bulk build infrastructure that checks on the health of the
[opam](https://opam.ocaml.org) package manager.

<p align="center">
  <img width="600" src="https://cdn.rawgit.com/ocaml/obi/1a9ad12aa756771b0ea731f40995c7ca7b55c31f/opam-ci.svg">
</p>

The main services and repositories associates with this infrastructure are:

- **Documentation:**
  - <https://github.com/ocaml/infrastructure/wiki>
  - <https://github.com/ocaml/infrastructure/wiki/Containers> is rebuilt automatically with the latest information
  - <https://github.com/ocaml/infrastructure/wiki/Using-the-opam-ci-tool>
- **GitHub:** Git repositories
  - <https://github.com/ocaml/obi>: for the source code
  - <https://github.com/ocaml/obi-logs>: for the build logs
- **Docker Hub:** container images
  - <https://hub.docker.com/r/ocaml/opam2>: opam2 and OCaml compiler images
  - <https://hub.docker.com/r/ocaml/opam2-staging>: intermediate container images for bulk builds
- **Coordination:**
  - <https://buildkite.com/ocaml>: the coordination Hub (account required until [buildkite#137](https://github.com/buildkite/feedback/issues/137) is resolved)

## Getting Started

The main tool you will want to try out is `opam-ci`, which provides
CLI access to build results. You can try this out by:

    opam update
    opam ci --help
    opam ci status --help
    opam ci logs --help

See the [online documentation](https://github.com/ocaml/infrastructure/wiki/Using-the-opam-ci-tool) for more information.

## Further Information

- [CHANGES.md](CHANGES.md) is the repository changelog for source code and
  Buildkite scripts.
- [METADATA.md](METADATA.md) contains a changelog for the Obi sexp format
  that is published on <https://github.com/ocaml/obi-logs/tree/index>.

While we are assembling the documentation, please contact @avsm for more information.
