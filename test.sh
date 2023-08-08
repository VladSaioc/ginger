#!/bin/sh

set -e fail

runIRExample () {
  DIR=$1
  FILE=$2
  EXT=$(echo "$DIR/$FILE" | sed "s/\//#/g")

  stack run -- -ir "$DIR/$FILE"
  ./dafny/dafny "./$DIR/$EXT.dfy"
}

runIRExample examples/simple 3-process-sync-2-receive
runIRExample examples/simple 3-process-sync-2-send
runIRExample examples/simple 4-process-sync