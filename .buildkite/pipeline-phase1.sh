#!/bin/bash
set -eu

echo "steps:"

cd $1
for i in Dockerfile.*; do
  f=$(echo $i | sed -e 's/^Dockerfile.//g') 
  echo "  - label: '$f'\n    command: \"echo processing $f\"\n"
done
