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
    - cd /home/opam/opam-repository
    - git pull origin master
    - opam admin upgrade
    - opam admin $cmd 2>&1 | tee $cmd.txt
    - buildkite-agent artifact upload $cmd.txt
  plugins:
    docker#v1.1.1:
      image: "ocaml/opam2-staging"
      always_pull: true
      env:
        OPAMCOLOR: "never"
EOL
}

cmd "lint"
cmd "cache"
cmd "check"

cat <<EOL
- wait
- label: "Push Results"
  command:
  - "ssh-add -D && ssh-add ~/.ssh/id_rsa.bulk"
  - "buildkite-agent artifact download '*.txt' ."
  - "rm -rf lints"
  - "git clone git@github.com:avsm/obi-logs lints --reference ."
  - "git -C lints checkout --orphan lints"
  - "git -C lints reset"
  - "git -C lints clean -dxf"
  - "cp *.txt lints/"
  - "git -C lints add ."
  - "git -C lints commit -m 'BuildKite Update'"
  - "git -C lints push origin lints -f"
  - "rm -rf lints *.txt"
  agents:
    githubpusher: "true"
EOL
