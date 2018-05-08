#!/bin/bash -e

rev=$1
srev=`echo $rev | cut -c1-6`
echo 'steps:'

cmd() {
  cmd=$1
  cat <<EOL
- agents:
    docker: "true"
    os: "linux"
  label: ":camel: opam admin $cmd"
  command:
    - git pull origin master
    - opam update
    - opam admin $cmd > $cmd.txt
    - buildkite artifact upload $cmd.txt
  plugins:
    docker#v1.1.1:
      image: "ocaml/opam2-staging"
      workdir: /home/opam/opam-repository
      always_pull: true
EOL
}

cmd "lint"
cmd "cache"

cat <<EOL
- wait
- label: "Push Results"
  command:
    - rm -rf obi-logs
    - git clone -b lints --depth=1 git://github.com/avsm/obi-logs
    - find obi-logs/ -type f
  agents:
    githubpusher: "true"
EOL
