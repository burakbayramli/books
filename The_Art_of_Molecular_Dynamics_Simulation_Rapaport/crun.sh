#! /bin/tcsh
set f=src/pr_$1_$2
echo "$f"
set b=`basename $f .c`
echo "$b"
gcc -O -I./src -o $b src/$b.c -lm
cp src/$b.in $b.in
./$b | tee $b.out
