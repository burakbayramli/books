#! /bin/tcsh
# install tcsh if missing, 'sudo apt install tcsh'
# for pr_08_1.c example, run with 'tcsh crun.sh 08 1'
set f=src/pr_$1_$2
echo "$f"
set b=`basename $f .c`
echo "$b"
gcc -O -I./src -o $b src/$b.c -lm
./$b data/$b.in $b.in
./$b | tee $b.out
