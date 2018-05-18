#!/bin/bash -e

rev=$1
srev=`echo $rev | cut -c1-6`
echo 'steps:'

build() {
  distro=$1
  ov=$2
  arch=$3
  cat <<EOL
- trigger: "opam-bulk"
  label: ":docker: $distro :camel: $ov :github: $srev :compute: $arch"
  build:
    env:
      DISTRO: "$distro"
      OCAML_VERSION: "$ov"
      OPAM_REPO_REV: "$rev"
      ARCH: "$arch"
EOL
}

index() {
  cat <<EOL
- wait
- trigger: "obi-index"
  label: "Update Obi Index"
EOL
}

build "debian-9" "4.07" "amd64"
build "debian-9" "4.06" "amd64"
build "debian-9" "4.05" "amd64"
build "debian-9" "4.04" "amd64"
build "debian-9" "4.03" "amd64"
build "debian-9" "4.08" "amd64"
build "debian-9" "4.06+default-unsafe-string" "amd64"
build "debian-9" "4.06+flambda" "amd64"
build "alpine-3.7" "4.06" "amd64"
build "ubuntu-18.04" "4.06" "amd64"
build "fedora-27" "4.06" "amd64"
index
