#!/bin/bash

set -e fail

runIRExample () {
  DIR=examples/$1

  for FILE in $(ls $DIR);
  do
    if [[ $FILE = "examples__"* ]];
    then
      echo "Skipping Dafny file $FILE"
    else
      EXT=$(echo "$DIR/$FILE" | sed "s/\//__/g")
      stack run -- -ir "$DIR/$FILE"
    fi
  done
}

runPromelaExample () {
  DIR=examples/gomelas/$1

  for FILE in $(ls $DIR);
  do
    if [[ $FILE = "examples__"* ]];
    then
      echo "Skipping Dafny file $FILE"
    else
      EXT=$(echo "$DIR/$FILE" | sed "s/\//__/g")
      stack run -- "$DIR/$FILE"
    fi
  done
  # ./dafny/dafny "./$DIR/$EXT.dfy"
}

runIRExample simple
runIRExample complex
runIRExample conditional
runIRExample return

runPromelaExample ast-transform
runPromelaExample conditional
runPromelaExample loop-body
runPromelaExample return
runPromelaExample simple

echo ""
echo "All tests executed successfully"
