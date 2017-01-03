#!/bin/sh

cp main.f.orig main.f
cp F77WAVE.fcp.orig F77WAVE.fcp

comment="original (optimal?) code"
echo "**************** $comment"
./fcpp.py F77WAVE.fcp
./compile.py app main.f F77WAVE.f input "$comment"

comment="formatted I/O"
echo "**************** $comment"
./fcpp.py F77WAVE.fcp
./compile.py app versions/main_wIO.f F77WAVE.f input "$comment"

comment="traversing the array rowwise"
echo "**************** $comment"
./fcpp.py versions/F77WAVE_columntraverse.fcp
./compile.py app main.f versions/F77WAVE_columntraverse.f input "$comment"

comment="lambda array replaced by h(0,0) calls"
echo "**************** $comment"
./fcpp.py versions/F77WAVE_call_h00.fcp
./compile.py app main.f versions/F77WAVE_call_h00.f input "$comment"

comment="lambda array replaced by h(x,y) calls"
echo "**************** $comment"
./fcpp.py versions/F77WAVE_call_hxy.fcp
./compile.py app main.f versions/F77WAVE_call_hxy.f input "$comment"

comment="if-test inside the double loop"
echo "**************** $comment"
./fcpp.py versions/F77WAVE_loop_wiftest.fcp
./compile.py app main.f versions/F77WAVE_loop_wiftest.f input "$comment"


