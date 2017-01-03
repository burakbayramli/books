#!/bin/sh -x

# testing the efficiency of reading 100000 (x,y) data points
# from file and writing (x,f(y)) data back to file again

# generate input data file:
python $scripting/src/efficiency/datatrans/xygenerator.py 0:10,0.0001 'x*x' > datatrans.tmp

# run Python scripts:
echo "datatrans1.py: plain Python"
time $scripting/src/py/intro/datatrans1.py datatrans.tmp tmp.1

echo "datatrans2.py: Python using plain lists"
time $scripting/src/py/intro/datatrans2.py datatrans.tmp tmp.1

echo "datatrans3a.py: Python w/NumPy arrays and filetable"
time $scripting/src/py/intro/datatrans3a.py datatrans.tmp tmp.1

echo "datatrans3b.py: Python w/NumPy arrays and TableIO"
time $scripting/src/py/intro/datatrans3b.py datatrans.tmp tmp.1

echo "datatrans3c.py: Python w/NumPy arrays and split of file.read()"
time $scripting/src/py/intro/datatrans3c.py datatrans.tmp tmp.1

echo "datatrans3d.py: Python w/NumPy arrays and Scientific.IO.ArrayIO"
time $scripting/src/py/intro/datatrans3d.py datatrans.tmp tmp.1

echo "datatrans1.pl: plain Perl"
time $scripting/src/perl/datatrans1.pl datatrans.tmp tmp.1

echo "datatrans1.tcl: plain Tcl"
time $scripting/src/tcl/datatrans1.tcl datatrans.tmp tmp.1

echo "compiling C and C++ codes in $scripting/src/misc/datatrans"
thisdir=`pwd`
cd $scripting/src/efficiency/datatrans/C
./make.sh
cd ../C++
./make.sh
cd $thisdir
echo
echo "datatrans1.app: plain C"
time $scripting/src/efficiency/datatrans/C/datatrans1.app datatrans.tmp tmp.1

echo "datatrans1.app: plain C++"
time $scripting/src/efficiency/datatrans/C++/datatrans1.app datatrans.tmp tmp.1
time $scripting/src/efficiency/datatrans/C++/datatrans1_eff.app datatrans.tmp tmp.1

# clean up:
#rm -f datatrans.tmp tmp.1 \
rm -f  $scripting/src/efficiency/datatrans/C/*.app \
  $scripting/src/efficiency/datatrans/C++/*.app





