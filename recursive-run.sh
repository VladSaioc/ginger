#!/bin/sh

recursiveVerify () {
  local dir=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      rm -rf $dir/$file/*.dfy
      rm -rf $dir/$file/*.res
      rm -rf $dir/$file/*.dll
      recursiveVerify $dir/$file
    else
      if [[ $file = *"-ginger.pml" ]]; then
        echo "Now testing: $dir/$file"
        timeout 360 stack run -- "$dir/$file" &> "$dir/$file-results.res"
      else
        echo "Skipping non-ginger input file $file"
      fi
    fi
  done
}

recursiveVerify $1
