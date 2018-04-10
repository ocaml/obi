#!/bin/bash
set -eu

echo "steps:"

cd $2/phase1-$1
for i in Dockerfile.*; do
  f=$(echo $i | sed -e 's/^Dockerfile.//g') 
  cat <<EOM
  - label: $f-$1
    command: "buildkite-agent artifact download phase1-$1/Dockerfile.$f . && cat phase1-$1/Dockerfile.$f && docker build -f phase1-$1/Dockerfile.$f -t $f phase1-$1"

EOM
done
#  echo "  - label: $f\n    command: \"buildkite-agent artifact download phase1-$1/Dockerfile.$f && cat phase1-$1/Dockerfile.$f && docker build -f phase1-$1/Dockerfile.$f -t $f\"\n"
