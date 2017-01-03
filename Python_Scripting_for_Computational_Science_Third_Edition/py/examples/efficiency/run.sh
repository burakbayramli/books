#!/bin/sh
python pyefficiency.py allocate range call flops matrixfill
echo 2.3 w O
python -O pyefficiency.py allocate range call flops matrixfill
echo 2.2
python2.2 pyefficiency.py allocate range call flops matrixfill
echo 2.2 w O
python2.2 -O pyefficiency.py allocate range call flops matrixfill
