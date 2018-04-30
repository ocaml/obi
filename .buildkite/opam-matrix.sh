#!/bin/bash -e

rev=$1
srev=`echo $rev | cut -c1-6`
echo 'steps:'

build() {
  distro=$1
  ov=$2
  cat <<EOL
- trigger: "opam-bulk"
  label: ":docker: $distro :camel: $ov :github: $srev"
  build:
    env:
      DISTRO: "$distro"
      OCAML_VERSION: "$ov"
      OPAM_REPO_REV: "$rev"
EOL
}

build "debian-9" "4.07"
build "debian-9" "4.06"
build "debian-9" "4.06+default-unsafe-string"
build "debian-9" "4.06+flambda"
build "alpine-3.7" "4.06"

cat <<EOL
- label: "Rebuild index"
  command:
    - rm -rf obi-logs
    - git clone --depth=1 git://github.com/avsm/obi-logs
    - find obi-logs/batch -type f
  agents:
    arch: "amd64"
    docker: "true"
    os: "linux"
EOL
