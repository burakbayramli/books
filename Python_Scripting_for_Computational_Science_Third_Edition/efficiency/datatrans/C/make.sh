#!/bin/sh

# determine compiler options (check first if the environment
# variable C_COMPILER is set):
if [ ! -n "$C_COMPILER" ]; then
  # base C_COMPILER on the current machine type:
  case `uname -s` in
	Linux)
		C_COMPILER=gcc
		C_COMPILER_OPTIONS="-Wall -O3"
		;;
	AIX)
		C_COMPILER=xlc
		C_COMPILER_OPTIONS="-O"
		;;
	*)
		C_COMPILER=gcc
		C_COMPILER_OPTIONS="-Wall -O3"
		;;
    esac
# else: C_COMPILER and C_COMPILER_OPTIONS are set in start-up file
fi
	
# fetch all C files:
files=`/bin/ls *.c`

for file in $files; do
  stem=`echo $file | sed 's/\.c$//'`
  echo $C_COMPILER $C_COMPILER_OPTIONS -o $stem.app $file -lm
  $C_COMPILER $C_COMPILER_OPTIONS -o $stem.app $file -lm
  ls -s $stem.app
done



