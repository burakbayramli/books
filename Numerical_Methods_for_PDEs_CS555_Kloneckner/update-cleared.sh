#! /usr/bin/env zsh

set -e
setopt -o EXTENDED_GLOB

with_echo()
{
  echo "$@"
  "$@"
}

unset PYTHONWARNINGS

ME=$(readlink -f "$0")
DIR=$(dirname "$ME")
MYDIR=$(cd "$DIR" && pwd)

INPUTDIR="$MYDIR/demos"
OUTPUTDIR="$MYDIR/cleared-demos"

for nb in $INPUTDIR/*/**/*.ipynb; do
  DIR="$(dirname "$nb")"
  BN="$(basename "$nb")"
  RELDIR="$(realpath --relative-to="$INPUTDIR" "$DIR")"

  CONV_DIR="$OUTPUTDIR/$RELDIR"
  mkdir -p "$CONV_DIR"
  CONV_IPYNB="$CONV_DIR/$BN"
  with_echo "$MYDIR/demos/ipython-demo-tools/prepare-ipynb" clear-output clear-marked-inputs "$nb" "$CONV_IPYNB"
done
