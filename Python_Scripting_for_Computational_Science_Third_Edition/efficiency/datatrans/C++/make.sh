#!/bin/sh

# determine compiler options (check first if the environment
# variable CPP_COMPILER is set):
if [ ! -n "$CPP_COMPILER" ]; then
  # base CPP_COMPILER on the current machine type:
  case `uname -s` in
	Linux)
		CPP_COMPILER=g++
		CPP_COMPILER_OPTIONS="-Wall -O3"
		;;
	AIX)
		CPP_COMPILER=xlC
		CPP_COMPILER_OPTIONS="-O"
		;;
	*)
		CPP_COMPILER=g++
		CPP_COMPILER_OPTIONS="-Wall -O3"
		;;
    esac
# else: CPP_COMPILER and CPP_COMPILER_OPTIONS are set in start-up file
fi
	
# fetch all C++ files:
files=`/bin/ls *.cpp`

# clean directory or compile? check 1st command-line arg:
if [ $# -lt 1 ]; then
  args="empty"
else
  args=$1
fi

for file in $files; do
  stem=`echo $file | sed 's/\.cpp$//'`
  echo $CPP_COMPILER $CPP_COMPILER_OPTIONS -o $stem.app $file -lm
  $CPP_COMPILER $CPP_COMPILER_OPTIONS -o $stem.app $file -lm
  ls -s $stem.app
done



