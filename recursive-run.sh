#!/bin/sh

recursiveVerify () {
  local dir=$1

  for file in $(ls $dir);
  do
    if [ -d $dir/$file ];
    then
      find . -name "$dir/$file/*.dfy" -type f -delete
      find . -name "$dir/$file/*.res" -type f -delete
      find . -name "$dir/$file/*.dll" -type f -delete
      find . -name "$dir/$file/*.runtimeconfig.json" -type f -delete
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
