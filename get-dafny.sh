#!/bin/sh

set -e

# Download and install dafny distribution
# Determine OS and assign it to OS
case $(uname) in
  "Linux")
    OS="ubuntu-20.04"
    ;;
  "Darwin")
    OS="macos-11"
    ;;
esac

# Determine architecture and assign it to ARCH
case $(arch) in
  "x86_64")
    ARCH="x64"
    ;;
esac


URL="https://github.com/dafny-lang/dafny/releases/download/v4.2.0/dafny-4.2.0-$ARCH-$OS.zip"
DAFZIP="dafny-4.2.0-$ARCH-$OS.zip"

[ ! -f "./$DAFZIP" ] && wget $URL

[ ! -d "./dafny" ] && unzip $DAFZIP
