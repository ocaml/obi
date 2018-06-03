#!/bin/bash -e

hub=${1:-ocaml/opam2-staging}
tag=${2:-obi-buildkite}
arches="amd64 arm64 ppc64le"

dockerfile_for_arch() {
  arch=$1
  DFILE="FROM ocaml/opam2-staging
RUN sudo apt-get update && sudo apt-get -y install m4 pkg-config
RUN opam switch 4.06
COPY . /home/opam/src/
RUN sudo chown -R opam /home/opam/src
RUN opam pin add -n --dev dockerfile-opam
RUN opam pin add -n --dev ocaml-version
RUN opam pin add -n --dev yaml
RUN opam install -y -j10 --deps-only /home/opam/src
RUN cd /home/opam/src && opam exec -- jbuilder build
COPY /home/opam/src/_build/install/default/bin/obi-buildkite /usr/bin/obi-buildkite
RUN chmod a+x /usr/bin/obi-buildkite
RUN rm -rf /home/opam/.opam /home/opam/src
"
  echo -n "  - \"echo "
  printf '%q' "$DFILE" | sed -e 's/^\$/$$/g'
  echo "> Dockerfile\""
}

build_one_arch() {
  arch=$1
  cat <<EOL
- label: "$arch"
  command:
EOL
  dockerfile_for_arch $arch
  cat <<EOL
  - cat Dockerfile
  - docker build --no-cache --pull -t $hub:$tag-$arch . && docker push $hub:$tag-$arch 
  agents:
    docker: "true"
    os: "linux"
    pusher: "true"
    arch: "$arch"
  plugins:
    docker-login#v1.0.0:
      username: avsm
EOL
}

echo steps:
for arch in $arches; do
  build_one_arch $arch
done
echo "- wait"
cat <<EOL
- label: "Multiarch"
  agents:
    docker: "true"
    os: "linux"
    pusher: "true"
    arch: "amd64"
  command:
EOL
for arch in $arches; do
  echo "  - docker pull $hub:$tag-$arch"
done
echo "  - docker manifest push -p $hub:$tag || true"
echo -n "  - docker manifest create $hub:$tag "
for arch in $arches; do
  echo -n "$hub:$tag-$arch "
done
echo 
for arch in $arches; do
  echo "  - docker manifest annotate $hub:$tag $hub:$tag-$arch --arch $arch"
done
echo "  - docker manifest inspect $hub:$tag"
echo "  - docker manifest push -p $hub:$tag"
