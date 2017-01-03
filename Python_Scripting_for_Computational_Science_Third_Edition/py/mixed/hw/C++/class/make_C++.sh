#!/bin/sh -x
# compile C++ code and make an executable tmp.app
# (use GNU g++)
g++ -o tmp.app -O HelloWorld.cpp main.cpp

# test:
./tmp.app 1.0 0

