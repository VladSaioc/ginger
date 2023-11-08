#!/bin/bash

set -e fail

verifyIRExample () {
  DIR=examples/$1

  for FILE in $(ls $DIR);
  do
    if [[ $FILE = "examples__"* ]];
    then
      echo "Skipping Dafny file $FILE"
    else
      echo "Now testing: $DIR/$FILE"
      EXT=$(echo "$DIR/$FILE" | sed "s/\//__/g")
      stack run -- -ir -color "$DIR/$FILE"
      echo "Success: $DIR/$FILE"
    fi
  done
}

translateIRExample () {
  DIR=examples/$1

  for FILE in $(ls $DIR);
  do
    if [[ $FILE = "examples__"* ]];
    then
      echo "Skipping Dafny file $FILE"
    else
      echo "Now testing: $DIR/$FILE"
      EXT=$(echo "$DIR/$FILE" | sed "s/\//__/g")
      stack run -- -ir -color -skip-verification "$DIR/$FILE"
      echo "Success: $DIR/$FILE"
    fi
  done
}

verifyPromelaExample () {
  DIR=examples/gomelas/$1

  for FILE in $(ls $DIR);
  do
    if [[ $FILE = "examples__"* ]];
    then
      echo "Skipping Dafny file $FILE"
    else
      echo "Now testing: $DIR/$FILE"
      EXT=$(echo "$DIR/$FILE" | sed "s/\//__/g")
      stack run -- -color "$DIR/$FILE"
      echo "Success: $DIR/$FILE"
    fi
  done
  # ./dafny/dafny "./$DIR/$EXT.dfy"
}

# Translation and verification of VIRGo
verifyIRExample simple
verifyIRExample complex
verifyIRExample conditional
verifyIRExample return
verifyIRExample sync-comm

# VIRGo translation only
translateIRExample go

# Translation and verification of Promela
verifyPromelaExample ast-transform
verifyPromelaExample conditional
verifyPromelaExample loop-body
verifyPromelaExample return
verifyPromelaExample simple

# Promela translation only
# --- Nothing here yet

echo ""
echo "All tests executed successfully"
