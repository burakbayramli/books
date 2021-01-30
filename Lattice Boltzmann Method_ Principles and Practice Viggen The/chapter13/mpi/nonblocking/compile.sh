#!/bin/bash

rm -f LBM.o seconds.o main.o sim

CXXFLAGS="-O3 -Wall"

mpic++ ${CXXFLAGS} -c LBM.cpp -o LBM.o
mpic++ ${CXXFLAGS} -c seconds.cpp -o seconds.o
mpic++ ${CXXFLAGS} -c main.cpp -o main.o
 
mpic++ LBM.o seconds.o main.o -o sim -lrt

# for mac:
# mpic++ LBM.o seconds.o main.o -o sim

