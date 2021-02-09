#!/bin/bash

rm -f sim

CXXFLAGS="-std=c++98 -pedantic -O3 -Wall"

g++ ${CXXFLAGS} seconds.cpp main.cpp -o sim -lrt

# for mac:
# g++ ${CXXFLAGS} seconds.cpp main.cpp -o sim

