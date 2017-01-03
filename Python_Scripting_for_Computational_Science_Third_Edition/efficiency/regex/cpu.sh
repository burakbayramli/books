#!/bin/sh
n=$1   # problem size
python makedata.py $n

echo "python regex reading"
time python regexread.py tmp1.dat

echo "perl regex reading"
time perl regexread.pl tmp1.dat

echo "python read+eval"
time python readeval.py tmp2.dat



