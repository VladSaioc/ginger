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
        if [ -f "$dir/$file-results.res" ]; then
          local ctnt=$(cat "$dir/$file-results.res")
          if [[ "$ctnt" = *"not parametric"* ]]; then
            continue
          fi
          if [[ "$ctnt" = *"unexpected"* ]]; then
            continue
          fi
        fi
        echo "Now testing: $dir/$file"
        timeout 360 stack run -- "$dir/$file" &> "$dir/$file-results.res"
      fi
    fi
  done
}

recursiveVerify $1
