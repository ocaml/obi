#!/bin/bash -ex
hub=$1
tag=$2
rev=$3
buildkite-agent artifact download "$tag/*" .
echo --- Building bulk image
docker build --no-cache --rm --pull -t $hub:$tag -f $tag/Dockerfile.$rev .
echo --- Pushing bulk image to Hub
docker push $hub:$tag
echo --- Listing packages to buikd
mkdir -p $tag
docker run $hub:$tag opam list -a -s | sort -R | head -5 > $tag/pkgs.txt
buildkite-agent artifact upload $tag/pkgs.txt
echo --- Generating and spawning build jobs
cat $tag/pkgs.txt | xargs -n 1 -I __NAME__ sh -c "sed -e 's/__PKG__/__NAME__/g' < $tag/template.yml > $tag/build-__NAME__.yml"
echo steps: > all.yml
(for i in `cat $tag/pkgs.txt`; do
  cat $tag/build-$i.yml;
 done) | grep -v ^steps >> all.yml
buildkite-agent pipeline upload all.yml
