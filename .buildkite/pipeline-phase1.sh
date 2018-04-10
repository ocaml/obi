#!/bin/bash
set -eu

echo "steps:"

cd $1
for i in Dockerfile.*; do
  f=$(echo $i | sed -e 's/^Dockerfile.//g') 
  echo "  - command: \"echo processing $f\""
done
