FROM ocaml/opam2:alpine-3.7-ocaml
ENV OPAMYES=1
RUN opam switch create .
RUN opam exec -- jbuilder build
