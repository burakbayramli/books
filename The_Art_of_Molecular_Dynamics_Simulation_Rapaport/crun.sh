#! /bin/tcsh
set f=src/pr_$1_$2
set b=`basename $f .c`
gcc -O -I./src -o $b src/$b.c -lm
cp data/$b.in $b.in
$b | tee $b.out
