#!/bin/sh -x
if [ $# -lt 1 ]; then
  n=1100
else
  n=$1
fi

#echo "Pure Python:"
# not necessary, repeated in Grid2Deff.py
#python $scripting/src/py/examples/Grid2D.py timing $n
#echo

cd F77
echo "F77 stand-alone program:"
perl -pi.old~ -e "s/^\s+parameter \(nmax=.*\)/      parameter (nmax=$n)/" gridloop.f
egrep "^\s+parameter \(nmax" gridloop.f
lines=`./make_F77_app.sh`
# extract CPU time (hardcoded with 50 repetitions):
best_time=`echo $lines | perl -ne 'printf("%.4f", $1/50.0) if /cpu=(.*)/;'`
echo
echo "BEST TIME (F77 stand-alone): $best_time"
./clean.sh

echo "F77 extension module:"
./make_module_1.sh > /dev/null
python ../Grid2Deff.py timing2 $n $best_time
./clean.sh

cd ../C/plain
echo "C extension module; a single handwritten function:"
./make_module_1.sh
python ../../Grid2Deff.py timing2 $n $best_time
./clean.sh

cd ../clibcall
echo "C extension module; separate function and handwritten wrapper:"
./make_module_1.sh
python ../../Grid2Deff.py timing2 $n $best_time
./clean.sh

cd ../../C++/plain
echo "C++ extension module; handwritten wrapper + class:"
./make_module_1.sh
python ../../Grid2Deff.py timing2 $n $best_time
./clean.sh

cd ../scxx
echo "C++ extension module; handwritten wrapper using SCXX:"
./make_module_1.sh
python ../../Grid2Deff.py timing2 $n $best_time
./clean.sh

cd ../convertptr
echo "C++ extension module; conversion class wrapped with SWIG:"
./make_module_1.sh
python Grid2Deff2.py timing3 $n $best_time
./clean.sh

echo "C++ standalone program (with MyArray and gridloop1):"
./make_Cpp_app.sh $n
./clean.sh


