#!/bin/bash

rm -f sim

# change sm_20 to match the architecture of your device
# allowed values are sm_Nn
# where N is the major version number and n is the minor number
# 'sm_20','sm_21','sm_30','sm_32','sm_35','sm_37','sm_50','sm_52','sm_53'
nvcc -arch sm_20 -v --ptxas-options=-v -O3 seconds.cpp main.cu -o sim

