#!/bin/bash

set -e fail

verifyIRExample () {
  local dir=examples/$1
  local curr=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      # echo "Directory $dir/$file"
      verifyIRExample $curr/$file
    else
      if [[ $file = "examples__"* ]];
      then
        echo "Skipping Dafny file $file"
      else
        # echo "File $dir/$file"
        echo "Now testing: $dir/$file"
        stack run -- -ir -color "$dir/$file"
        echo "Success: $dir/$file"
      fi
    fi
  done
}

translateIRExample () {
  local dir=examples/$1
  local curr=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      translateIRExample $curr/$file
    else
      if [[ $file = "examples__"* ]];
      then
        echo "Skipping Dafny file $file"
      else
        echo "Now testing: $dir/$file"
        stack run -- -ir -color -skip-verification "$dir/$file"
        echo "Success: $dir/$file"
      fi
    fi
  done
}

verifyPromelaExample () {
  local dir=examples/gomelas/$1
  local curr=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      verifyPromelaExample $curr/$file
    else
      if [[ $file = "examples__"* ]];
      then
        echo "Skipping Dafny file $file"
      else
        echo "Now testing: $dir/$file"
        stack run -- -color "$dir/$file"
        echo "Success: $dir/$file"
      fi
    fi
  done
}

# Translation and verification of VIRGo
verifyIRExample good

# VIRGo translation only
translateIRExample translatable

# Translation and verification of Promela
verifyPromelaExample good

# Promela translation only
# --- Nothing here yet

echo ""
echo "All tests executed successfully"
