#!/bin/bash

rm -f LBM.o seconds.o main.o sim

CXXFLAGS="-std=c++98 -pedantic -O3 -Wall"

g++ ${CXXFLAGS} -fopenmp -c LBM.cpp -o LBM.o
g++ ${CXXFLAGS} -fopenmp -c seconds.cpp -o seconds.o
g++ ${CXXFLAGS} -fopenmp -c main.cpp -o main.o
 
g++ -fopenmp LBM.o seconds.o main.o -o sim -lrt

# for mac:
# g++ -fopenmp LBM.o seconds.o main.o -o sim

