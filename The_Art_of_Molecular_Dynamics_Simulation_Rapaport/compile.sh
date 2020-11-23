# script for testing that programs compile correctly (see README)

#! /bin/tcsh

#set CFLAGS = "-I./src -I/usr/include/openmpi-x86_64"
set CFLAGS = "-I./src"

#set LFLAGS = "-L/usr/lib64/openmpi/lib -lmpi -lpthread -lm"
set LFLAGS = "-lm"

foreach f ( src/pr_* )

  echo $f
  gcc $CFLAGS $f $LFLAGS
  rm a.out

end
