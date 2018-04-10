#!/bin/bash
set -eu

echo "steps:"

cd $2/phase1-$1
for i in Dockerfile.*; do
  f=$(echo $i | sed -e 's/^Dockerfile.//g') 
  cat <<EOM
  - label: $f-$1
    agent:
      arch: $1
    command:
      - buildkite-agent artifact download phase1-$1/Dockerfile.$f .
      - cat phase1-$1/Dockerfile.$f
      - docker build --no-cache --pull -f phase1-$1/Dockerfile.$f -t ocaml/opam2-staging:$f-opam-linux-$1 phase1-$1
    plugins:
      docker-login#v1.0.0:
        username: avsm
EOM
done

echo "  - wait"


