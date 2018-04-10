#!/bin/sh

cmd() {
    echo "\$ $@"
    $@
}

export OPAMERRLOGLEN=0
export OPAMYES=1
export OPAMJSON=$1.json

starttime=`date +%s`
cmd opam depext -iy -j 2 $1 > $1.txt 2>&1
exitcode=$?
endtime=`date +%s`
echo $exitcode $starttime $endtime >> $1.txt
sudo cp $1.json /mnt/$1.json
sudo cp $1.txt /mnt/$1.txt
