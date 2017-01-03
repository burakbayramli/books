#!/bin/sh -x
./clean.sh

g++ -O3 -o tmp.app -I. main.cpp gridloop.cpp MyArray.cpp
timer.py ./tmp.app $1
